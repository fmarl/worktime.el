;;; worktime.el --- Calculate the remaining time for the current working day -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://github.com/fmarl/worktime.el

;;; Commentary:

;; `worktime-start' records when the working day began and enables
;; `worktime-mode', which shows the remaining time and the end of the
;; day in the mode line, updated once per minute.

;;; Code:

(defgroup worktime nil
  "Show the remaining worktime in the mode line."
  :group 'tools
  :prefix "worktime-")

(defcustom worktime-hours-per-week 40
  "Hours of work per week."
  :type 'number
  :group 'worktime)

(defcustom worktime-days-per-week 5
  "Working days per week."
  :type 'integer
  :group 'worktime)

(defcustom worktime-break-per-day 30
  "Break per day in minutes."
  :type 'integer
  :group 'worktime)

(defconst worktime--max-start-lead (* 6 60 60)
  "Seconds a start time may lie ahead of now before it is read as past.")

(defvar worktime--start-time nil
  "Start of the working day as a Lisp timestamp, or nil.")

(defvar worktime--modeline-string "")

(defvar worktime--timer nil)

(defun worktime--minutes-per-workday ()
  "Minutes between start and end of a working day, including the break."
  (+ (round (/ (* worktime-hours-per-week 60.0) worktime-days-per-week))
     worktime-break-per-day))

(defun worktime--hhmm (minutes)
  "Format MINUTES as HH:MM."
  (format "%02d:%02d" (/ minutes 60) (mod minutes 60)))

(defun worktime--resolve-start (hour minute)
  "Return the timestamp of the start time HOUR:MINUTE."
  (let* ((now (current-time))
         (decoded (decode-time now))
         (day (* 24 60 60)))
    (setf (decoded-time-second decoded) 0
          (decoded-time-minute decoded) minute
          (decoded-time-hour decoded) hour
          (decoded-time-dst decoded) -1
          (decoded-time-zone decoded) nil)
    (let ((lead (float-time (time-subtract (encode-time decoded) now))))
      (cond ((> lead worktime--max-start-lead)
             (setf (decoded-time-day decoded) (1- (decoded-time-day decoded))))
            ((<= lead (- worktime--max-start-lead day))
             (setf (decoded-time-day decoded) (1+ (decoded-time-day decoded))))))
    (encode-time decoded)))

(defun worktime--end-time ()
  "End of the working day as a Lisp timestamp."
  (time-add worktime--start-time (* 60 (worktime--minutes-per-workday))))

(defun worktime--remaining-string ()
  "Remaining time as -HH:MM, or +HH:MM once the day is over."
  (let* ((elapsed (floor (float-time (time-subtract nil worktime--start-time)) 60))
         (remaining (- (worktime--minutes-per-workday) elapsed)))
    (concat (if (> remaining 0) "-" "+") (worktime--hhmm (abs remaining)))))

(defun worktime--update ()
  "Refresh the mode line string."
  (setq worktime--modeline-string
        (if worktime--start-time
            (format " %s %s"
                    (worktime--remaining-string)
                    (format-time-string "%H:%M" (worktime--end-time)))
          ""))
  (force-mode-line-update t))

(defun worktime--start-timer ()
  "Update once per minute, aligned to the next full minute."
  (setq worktime--timer
        (run-at-time (- 60 (nth 0 (decode-time))) 60 #'worktime--update)))

(defun worktime--stop-timer ()
  "Cancel the update timer."
  (when worktime--timer
    (cancel-timer worktime--timer)
    (setq worktime--timer nil)))

;;;###autoload
(define-minor-mode worktime-mode
  "Show the remaining worktime in the mode line."
  :lighter (:eval worktime--modeline-string)
  :global t
  (worktime--stop-timer)
  (when worktime-mode
    (worktime--start-timer)
    (worktime--update)))

;;;###autoload
(defun worktime-start (hour minute)
  "Start the working day at HOUR:MINUTE and enable `worktime-mode'."
  (interactive "nStart hour: \nnStart minute: ")
  (unless (and (natnump hour) (< hour 24) (natnump minute) (< minute 60))
    (user-error "Invalid start time: %s:%s" hour minute))
  (setq worktime--start-time (worktime--resolve-start hour minute))
  (worktime-mode 1))

(provide 'worktime)
;;; worktime.el ends here
