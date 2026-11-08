;;; worktime.el --- Track the working day and show its remaining time -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>
;; Version: 0.4
;; Package-Requires: ((emacs "29.1"))
;; Keywords: calendar, tools
;; URL: https://github.com/fmarl/worktime.el

;;; Commentary:

;; `worktime-start' begins a working day and enables `worktime-mode',
;; which shows the remaining time and the end of the day in the mode
;; line.  `worktime-pause', `worktime-resume' and `worktime-add-break'
;; record breaks, `worktime-stop' ends the day.
;;
;; Days are stored in `worktime-file'.  Recorded breaks count toward
;; the break required by `worktime-required-break-function'.  Whatever
;; is still missing is added to the end of the day.

;;; Code:

(require 'cl-lib)
(require 'parse-time)
(require 'time-date)

(defgroup worktime nil
  "Track the working day and show its remaining time."
  :group 'tools
  :prefix "worktime-")

(defcustom worktime-hours-per-week 40
  "Hours of work per week."
  :type 'number)

(defcustom worktime-days-per-week 5
  "Working days per week."
  :type '(natnum :match (lambda (_widget value)
                          (and (natnump value) (> value 0)))))

(defun worktime-required-break-arbzg (minutes)
  "Return the break due after MINUTES of work under § 4 ArbZG."
  (cond ((> minutes (* 9 60)) 45)
        ((> minutes (* 6 60)) 30)
        (t 0)))

(defcustom worktime-required-break-function #'worktime-required-break-arbzg
  "Function mapping minutes of work to the break due in minutes.
The result must not decrease as the minutes of work increase."
  :type '(choice (function-item worktime-required-break-arbzg) function))

(defcustom worktime-min-break-minutes 15
  "Minimum length of a break counted toward the break due, see § 4 ArbZG."
  :type 'natnum)

(defcustom worktime-max-minutes-per-day (* 10 60)
  "Maximum minutes of work per day, see § 3 ArbZG."
  :type 'natnum)

(defcustom worktime-min-rest-minutes (* 11 60)
  "Minimum rest in minutes between two working days, see § 5 ArbZG."
  :type 'natnum)

(defcustom worktime-file (locate-user-emacs-file "worktime.eld")
  "File in which working days are stored."
  :type 'file)

(defcustom worktime-start-functions nil
  "Functions called with the new `worktime-day' after `worktime-start'."
  :type 'hook)

(defcustom worktime-stop-functions nil
  "Functions called with the finished `worktime-day' after `worktime-stop'."
  :type 'hook)

(defface worktime-limit-exceeded '((t :inherit warning))
  "Face of the mode line text once `worktime-max-minutes-per-day' is reached.")

(cl-defstruct worktime-day
  "A working day."
  start
  (end nil :documentation "nil while the day is open.")
  (breaks nil :documentation "List of `worktime-break', oldest first.")
  (break-minutes nil :documentation "Total break, set by `worktime-stop'."))

(cl-defstruct worktime-break
  "A break."
  start
  (end nil :documentation "nil while the break is running."))

(defconst worktime--max-start-lead (* 6 60)
  "Minutes a start time may lie in the future before it means yesterday.")

(defvar worktime--day nil
  "The open `worktime-day' shown in the mode line, or nil.")

(defvar worktime-mode-line-string ""
  "String shown in `global-mode-string' while `worktime-mode' is enabled.")

;; The mode line drops text properties of variables not marked risky
(put 'worktime-mode-line-string 'risky-local-variable t)

(defvar worktime--timer nil)

;;;; Time helpers

(defun worktime--hhmm (minutes)
  "Format MINUTES as HH:MM."
  (format "%02d:%02d" (/ minutes 60) (mod minutes 60)))

(defun worktime--minutes-between (from to)
  "Return the whole minutes from FROM to TO."
  (floor (float-time (time-subtract to from)) 60))

(defun worktime--parse-hhmm (string)
  "Parse STRING as HH:MM into (HOUR . MINUTE)."
  (pcase string
    ((and (rx bos (let hour (** 1 2 digit)) (? ":") (let minute (= 2 digit))
              eos)
          (guard (< (string-to-number hour) 24))
          (guard (< (string-to-number minute) 60)))
     (cons (string-to-number hour) (string-to-number minute)))
    (_ (user-error "Invalid time: %s" string))))

(defun worktime--time-on (hour minute base &optional day-offset)
  "Return HOUR:MINUTE on the day of BASE, shifted by DAY-OFFSET days."
  (let ((decoded (decoded-time-add (decode-time base)
                                   (make-decoded-time :day (or day-offset 0)))))
    (setf (decoded-time-second decoded) 0
          (decoded-time-minute decoded) minute
          (decoded-time-hour decoded) hour
          (decoded-time-dst decoded) -1
          (decoded-time-zone decoded) nil)
    (encode-time decoded)))

(defun worktime--time-after (hour minute base)
  "Return the first time at HOUR:MINUTE that is not before BASE."
  (let ((time (worktime--time-on hour minute base)))
    (if (time-less-p time base)
        (worktime--time-on hour minute base 1)
      time)))

(defun worktime--resolve-start (hour minute now)
  "Return the start time HOUR:MINUTE as entered at NOW."
  (let* ((time (worktime--time-on hour minute now))
         (lead (worktime--minutes-between now time)))
    (cond ((> lead worktime--max-start-lead)
           (worktime--time-on hour minute now -1))
          ((<= lead (- worktime--max-start-lead (* 24 60)))
           (worktime--time-on hour minute now 1))
          (t time))))

;;;; Days

(defun worktime--open-break (day)
  "Return the running break of DAY, or nil."
  (cl-find nil (worktime-day-breaks day) :key #'worktime-break-end))

(defun worktime--close-break (day time)
  "Return a copy of DAY with its running break ended at TIME."
  (let ((day (copy-worktime-day day)))
    (setf (worktime-day-breaks day)
          (mapcar (lambda (break)
                    (if (worktime-break-end break)
                        break
                      (make-worktime-break :start (worktime-break-start break)
                                           :end time)))
                  (worktime-day-breaks day)))
    day))

(defun worktime--add-break (day break now)
  "Return a copy of DAY with BREAK added at NOW.
Signal a `user-error' if BREAK overlaps another break, lies outside
DAY or ends in the future."
  (let ((start (worktime-break-start break))
        (end (worktime-break-end break)))
    (when (time-less-p start (worktime-day-start day))
      (user-error "Break starts before the working day"))
    (when (and end (not (time-less-p start end)))
      (user-error "Break ends before it starts"))
    (when (and end (time-less-p now end))
      (user-error "Break ends in the future"))
    (dolist (other (worktime-day-breaks day))
      (when (and (time-less-p start (or (worktime-break-end other) now))
                 (time-less-p (worktime-break-start other) (or end now)))
        (user-error "Break overlaps the one from %s"
                    (format-time-string "%R" (worktime-break-start other)))))
    (let ((day (copy-worktime-day day)))
      (setf (worktime-day-breaks day)
            (sort (cons break (copy-sequence (worktime-day-breaks day)))
                  (lambda (a b)
                    (time-less-p (worktime-break-start a)
                                 (worktime-break-start b)))))
      day)))

;;;; Calculation

(defun worktime--minutes-per-workday ()
  "Return the minutes of work per day, excluding breaks."
  (round (/ (* worktime-hours-per-week 60.0) worktime-days-per-week)))

(defun worktime--break-minutes (breaks)
  "Return (RECORDED . CREDITED) minutes of the finished BREAKS.
CREDITED only counts breaks of at least `worktime-min-break-minutes'."
  (cl-loop for break in breaks
           for minutes = (worktime--minutes-between (worktime-break-start break)
                                                    (worktime-break-end break))
           sum minutes into recorded
           when (>= minutes worktime-min-break-minutes)
           sum minutes into credited
           finally return (cons recorded credited)))

(defun worktime--owed-break (work credited)
  "Return the break still due after WORK minutes with CREDITED taken."
  (max 0 (- (funcall worktime-required-break-function work) credited)))

(defun worktime--worked-minutes (day now)
  "Return the minutes worked on DAY up to NOW, excluding breaks."
  (pcase-let* ((elapsed (worktime--minutes-between (worktime-day-start day)
                                                 now))
               (breaks (worktime-day-breaks (worktime--close-break day now)))
               (`(,recorded . ,credited) (worktime--break-minutes breaks))
               (work (- elapsed recorded)))
    (while (and (> work 0)
                (> (+ work recorded (worktime--owed-break work credited))
                   elapsed))
      (cl-decf work))
    work))

(defun worktime--end-time (day now)
  "Return the end of DAY as calculated at NOW."
  (pcase-let ((work (worktime--minutes-per-workday))
              (`(,recorded . ,credited)
               (worktime--break-minutes
                (worktime-day-breaks (worktime--close-break day now)))))
    (time-add (worktime-day-start day)
              (* 60 (+ work recorded (worktime--owed-break work credited))))))

(defun worktime--overdue-p (day now)
  "Return non-nil if DAY lasts longer at NOW than a working day may.
A running break counts as working time."
  (let ((ended (cl-remove nil (worktime-day-breaks day)
                          :key #'worktime-break-end)))
    (> (worktime--minutes-between (worktime-day-start day) now)
       (+ worktime-max-minutes-per-day
          (funcall worktime-required-break-function
                   worktime-max-minutes-per-day)
          (car (worktime--break-minutes ended))))))

;;;; Storage

(defun worktime--encode-time (time)
  "Return TIME as an ISO 8601 string, or nil."
  (and time (format-time-string "%FT%T%:z" time)))

(defun worktime--decode-time (string)
  "Parse the ISO 8601 STRING, or return nil."
  (and string (parse-iso8601-time-string string)))

(defun worktime--day-to-plist (day)
  "Return DAY in the form stored in `worktime-file'."
  (list :start (worktime--encode-time (worktime-day-start day))
        :end (worktime--encode-time (worktime-day-end day))
        :breaks (mapcar (lambda (break)
                          (cons (worktime--encode-time
                                 (worktime-break-start break))
                                (worktime--encode-time
                                 (worktime-break-end break))))
                        (worktime-day-breaks day))
        :break (worktime-day-break-minutes day)))

(defun worktime--day-from-plist (plist)
  "Return the `worktime-day' stored as PLIST."
  (make-worktime-day
   :start (worktime--decode-time (plist-get plist :start))
   :end (worktime--decode-time (plist-get plist :end))
   :breaks (mapcar (lambda (break)
                     (make-worktime-break
                      :start (worktime--decode-time (car break))
                      :end (worktime--decode-time (cdr break))))
                   (plist-get plist :breaks))
   :break-minutes (plist-get plist :break)))

(defun worktime--read-days ()
  "Return the days in `worktime-file', oldest first."
  (with-temp-buffer
    (ignore-error file-missing
      (insert-file-contents worktime-file))
    (skip-chars-forward " \t\n")
    (unless (eobp)
      (mapcar #'worktime--day-from-plist (read (current-buffer))))))

(defun worktime--save (day)
  "Store DAY in `worktime-file', replacing the day with the same start."
  (let ((days (sort (cons day (cl-remove (worktime-day-start day)
                                         (worktime--read-days)
                                         :key #'worktime-day-start
                                         :test #'time-equal-p))
                    (lambda (a b)
                      (time-less-p (worktime-day-start a)
                                   (worktime-day-start b))))))
    (make-directory (file-name-directory (expand-file-name worktime-file)) t)
    (with-temp-file worktime-file
      (let ((print-length nil)
            (print-level nil)
            (indent-tabs-mode nil))
        (pp (mapcar #'worktime--day-to-plist days) (current-buffer))))))

(defun worktime--open-day (&optional days)
  "Return the day of DAYS that has not ended yet, or nil.
DAYS defaults to the days in `worktime-file'."
  (let ((day (car (last (or days (worktime--read-days))))))
    (and day (not (worktime-day-end day)) day)))

(defun worktime--require-open-day ()
  "Return the open day or signal a `user-error'."
  (or (worktime--open-day) (user-error "No working day is open")))

;;;; Mode line

(defun worktime--mode-line-string (day now)
  "Return the mode line text for DAY at NOW."
  (let* ((worked (worktime--worked-minutes day now))
         (remaining (- (worktime--minutes-per-workday) worked))
         (break (worktime--open-break day)))
    (propertize
     (concat " "
             (and break
                  (format "P %s " (worktime--hhmm
                                   (worktime--minutes-between
                                    (worktime-break-start break) now))))
             (if (> remaining 0) "-" "+")
             (worktime--hhmm (abs remaining))
             " "
             (format-time-string "%H:%M" (worktime--end-time day now)))
     'face (and (>= worked worktime-max-minutes-per-day)
                'worktime-limit-exceeded))))

(defun worktime--update ()
  "Refresh the mode line string."
  (setq worktime-mode-line-string
        (if worktime--day
            (worktime--mode-line-string worktime--day (current-time))
          ""))
  (force-mode-line-update t))

(defun worktime--set-day (day)
  "Show DAY in the mode line."
  (setq worktime--day day)
  (worktime--update))

(defun worktime--start-timer ()
  "Update the mode line at every full minute."
  (setq worktime--timer
        (run-at-time (- 60 (nth 0 (decode-time))) 60 #'worktime--update)))

(defun worktime--stop-timer ()
  "Cancel the update timer."
  (when worktime--timer
    (cancel-timer worktime--timer)
    (setq worktime--timer nil)))

;;;###autoload
(define-minor-mode worktime-mode
  "Show the remaining worktime in the mode line.
Enabling the mode resumes the open day from `worktime-file'."
  :global t
  (worktime--stop-timer)
  (or global-mode-string (setq global-mode-string '("")))
  (if worktime-mode
      (progn
        (when (> (worktime--minutes-per-workday) worktime-max-minutes-per-day)
          (display-warning
           'worktime
           (format "Working day of %s exceeds the maximum of %s"
                   (worktime--hhmm (worktime--minutes-per-workday))
                   (worktime--hhmm worktime-max-minutes-per-day))))
        (add-to-list 'global-mode-string 'worktime-mode-line-string t)
        (worktime--start-timer)
        (worktime--set-day
         (condition-case err
             (worktime--open-day)
           (error
            (display-warning 'worktime
                             (format "Cannot read %s: %s" worktime-file
                                     (error-message-string err))
                             :error)
            nil))))
    (setq global-mode-string
          (delq 'worktime-mode-line-string global-mode-string))))

;;;; Reading arguments

(defun worktime--read-hhmm (prompt default)
  "Read HH:MM with PROMPT, offering the timestamp DEFAULT."
  (let ((default (format-time-string "%H:%M" default)))
    (worktime--parse-hhmm
     (read-string (format-prompt prompt default) nil nil default))))

(defun worktime--read-time-after (prompt base default)
  "Read HH:MM with PROMPT as the first such time not before BASE.
DEFAULT is the timestamp offered."
  (pcase-let ((`(,hour . ,minute) (worktime--read-hhmm prompt default)))
    (worktime--time-after hour minute base)))

(defun worktime--read-stop-args (day now)
  "Read the arguments of `worktime-stop' for DAY at NOW.
Offer NOW as default, or the calculated ends if DAY is overdue."
  (let* ((overdue (worktime--overdue-p day now))
         (break (worktime--open-break day))
         (break-end
          (and break
               (worktime--read-time-after
                "End of break" (worktime-break-start break)
                (if overdue
                    (time-add (worktime-break-start break)
                              (* 60 (funcall worktime-required-break-function
                                             (worktime--minutes-per-workday))))
                  now))))
         (closed (if break (worktime--close-break day break-end) day)))
    (list (worktime--read-time-after
           (format-time-string "End of working day started %a %R"
                               (worktime-day-start day))
           (or break-end (worktime-day-start day))
           (if overdue (worktime--end-time closed now) now))
          break-end)))

;;;; Commands

;;;###autoload
(defun worktime-start (start &optional previous-end previous-break-end)
  "Start a working day at START and enable `worktime-mode'.
If a day is still open, first stop it at PREVIOUS-END and its
running break at PREVIOUS-BREAK-END.  Interactively, read the
times as HH:MM."
  (interactive
   (let* ((now (current-time))
          (open (worktime--open-day))
          (stop-args (and open (worktime--read-stop-args open now))))
     (pcase-let ((`(,hour . ,minute) (worktime--read-hhmm "Start" now)))
       (cons (worktime--resolve-start hour minute now) stop-args))))
  (let* ((days (worktime--read-days))
         (open (worktime--open-day days))
         (end (if open
                  previous-end
                (and days (worktime-day-end (car (last days)))))))
    (when (and open (not previous-end))
      (user-error "Working day from %s is still open"
                  (format-time-string "%F %R" (worktime-day-start open))))
    (when (and end (time-less-p start end))
      (user-error "Previous day ended later, at %s"
                  (format-time-string "%F %R" end)))
    (when open
      (worktime--stop-day open previous-end previous-break-end
                          (current-time)))
    (let ((rest (and end (worktime--minutes-between end start))))
      (when (and rest (< rest worktime-min-rest-minutes))
        (display-warning
         'worktime
         (format "Only %s of rest since %s, minimum is %s"
                 (worktime--hhmm rest)
                 (format-time-string "%F %R" end)
                 (worktime--hhmm worktime-min-rest-minutes))))))
  (let ((day (make-worktime-day :start start)))
    (worktime--save day)
    (run-hook-with-args 'worktime-start-functions day)
    (worktime-mode 1)))

(defun worktime--stop-day (day end break-end now)
  "Stop DAY at END and its running break at BREAK-END or END.
END must not lie after NOW.  Save the stopped day, run
`worktime-stop-functions' and return it."
  (let ((day (worktime--close-break day (or break-end end))))
    (when (time-less-p end (worktime-day-start day))
      (user-error "Working day ends before it started"))
    (when (time-less-p now end)
      (user-error "Working day ends in the future"))
    (dolist (break (worktime-day-breaks day))
      (when (time-less-p (worktime-break-end break)
                         (worktime-break-start break))
        (user-error "Break ends before it starts"))
      (when (time-less-p end (worktime-break-end break))
        (user-error "Working day ends before its last break")))
    (setf (worktime-day-end day) end
          (worktime-day-break-minutes day)
          (- (worktime--minutes-between (worktime-day-start day) end)
             (worktime--worked-minutes day end)))
    (worktime--save day)
    (run-hook-with-args 'worktime-stop-functions day)
    day))

;;;###autoload
(defun worktime-stop (end &optional break-end)
  "Stop the open working day at END.
End a running break at BREAK-END, or at END if BREAK-END is nil."
  (interactive (worktime--read-stop-args (worktime--require-open-day)
                                         (current-time)))
  (worktime--stop-day (worktime--require-open-day) end break-end
                      (current-time))
  (worktime--set-day nil))

(defun worktime--commit (day)
  "Save DAY and show it in the mode line."
  (worktime--save day)
  (worktime--set-day day))

;;;###autoload
(defun worktime-pause ()
  "Start a break now."
  (interactive)
  (let ((day (worktime--require-open-day))
        (now (current-time)))
    (when (worktime--open-break day)
      (user-error "A break is already running"))
    (worktime--commit
     (worktime--add-break day (make-worktime-break :start now) now))))

;;;###autoload
(defun worktime-resume ()
  "End the running break now."
  (interactive)
  (let ((day (worktime--require-open-day)))
    (unless (worktime--open-break day)
      (user-error "No break is running"))
    (worktime--commit (worktime--close-break day (current-time)))))

;;;###autoload
(defun worktime-add-break (start end)
  "Record a break from START to END on the open day."
  (interactive
   (let* ((now (current-time))
          (start (worktime--read-time-after
                  "Break start"
                  (worktime-day-start (worktime--require-open-day))
                  now)))
     (list start (worktime--read-time-after "Break end" start now))))
  (worktime--commit
   (worktime--add-break (worktime--require-open-day)
                        (make-worktime-break :start start :end end)
                        (current-time))))

(provide 'worktime)
;;; worktime.el ends here
