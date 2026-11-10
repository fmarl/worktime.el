;;; worktime-test.el --- Tests for worktime -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'worktime)

(defun worktime-test--time (time &optional date)
  "Return the local TIME (HH:MM) on DATE, default 2026-11-09."
  (encode-time (parse-time-string
                (format "%s %s:00" (or date "2026-11-09") time))))

(defun worktime-test--fmt (time)
  (format-time-string "%F %R" time))

(defun worktime-test--day (start &rest breaks)
  "Return an open day from START with BREAKS as (START . END) strings."
  (make-worktime-day
   :start (worktime-test--time start)
   :breaks (mapcar (lambda (break)
                     (make-worktime-break
                      :start (worktime-test--time (car break))
                      :end (and (cdr break)
                                (worktime-test--time (cdr break)))))
                   breaks)))

(defmacro worktime-test--at (time &rest body)
  "Run BODY with the current time fixed to TIME, a timestamp."
  (declare (indent 1))
  `(let ((now ,time))
     (cl-letf (((symbol-function 'current-time) (lambda () now)))
       ,@body)))

(defmacro worktime-test--with-defaults (&rest body)
  "Run BODY with the default settings."
  (declare (indent 0))
  `(let ((worktime-hours-per-week 40)
         (worktime-days-per-week 5)
         (worktime-required-break-function #'worktime-required-break-arbzg)
         (worktime-min-break-minutes 15)
         (worktime-max-minutes-per-day 600)
         (worktime-min-rest-minutes 660))
     ,@body))

(defmacro worktime-test--with-file (&rest body)
  "Run BODY with default settings and an empty `worktime-file'.
The current time is 2026-11-11 12:00 unless BODY changes it."
  (declare (indent 0))
  `(worktime-test--with-defaults
     (let ((worktime-file (make-temp-file "worktime-test-" nil ".eld"))
           (worktime-timedot-file nil)
           (worktime-start-functions nil)
           (worktime-stop-functions nil)
           (worktime--day nil)
           (global-mode-string nil))
       (unwind-protect
           (worktime-test--at (worktime-test--time "12:00" "2026-11-11")
             (cl-letf (((symbol-function 'display-warning) #'ignore))
               ,@body))
         (worktime-mode -1)
         (delete-file worktime-file)))))

(defun worktime-test--worked (day now)
  (worktime-test--with-defaults
    (worktime--worked-minutes day (worktime-test--time now))))

(defun worktime-test--end (day &optional now)
  (worktime-test--with-defaults
    (format-time-string "%R" (worktime--end-time
                              day (worktime-test--time (or now "08:00"))))))

;;;; Calculation

(ert-deftest worktime-test-minutes-per-workday ()
  (let ((worktime-hours-per-week 40)
        (worktime-days-per-week 5))
    (should (= (worktime--minutes-per-workday) 480)))
  (let ((worktime-hours-per-week 38.5)
        (worktime-days-per-week 5))
    (should (= (worktime--minutes-per-workday) 462))))

(ert-deftest worktime-test-break-arbzg ()
  (should (= (worktime-required-break-arbzg 360) 0))
  (should (= (worktime-required-break-arbzg 361) 30))
  (should (= (worktime-required-break-arbzg 540) 30))
  (should (= (worktime-required-break-arbzg 541) 45)))

(ert-deftest worktime-test-hhmm ()
  (should (equal (worktime--hhmm 0) "00:00"))
  (should (equal (worktime--hhmm 510) "08:30")))

(ert-deftest worktime-test-parse-hhmm ()
  (should (equal (worktime--parse-hhmm "8:15") '(8 . 15)))
  (should (equal (worktime--parse-hhmm "0815") '(8 . 15)))
  (should-error (worktime--parse-hhmm "24:00") :type 'user-error)
  (should-error (worktime--parse-hhmm "8:60") :type 'user-error)
  (should-error (worktime--parse-hhmm "abc") :type 'user-error))

(ert-deftest worktime-test-time-after ()
  (let ((base (worktime-test--time "22:00")))
    (should (equal (worktime-test--fmt (worktime--time-after 23 0 base))
                   "2026-11-09 23:00"))
    (should (equal (worktime-test--fmt (worktime--time-after 6 0 base))
                   "2026-11-10 06:00"))))

(ert-deftest worktime-test-time-after-across-dst-change ()
  (let ((process-environment (cons "TZ=Europe/Berlin" process-environment)))
    (should (equal (format-time-string
                    "%F %R %z"
                    (worktime--time-after
                     6 0 (worktime-test--time "22:00" "2026-10-24")))
                   "2026-10-25 06:00 +0100"))))

(ert-deftest worktime-test-resolve-start ()
  (let ((now (worktime-test--time "10:00")))
    (should (equal (worktime-test--fmt (worktime--resolve-start 8 15 now))
                   "2026-11-09 08:15"))
    (should (equal (worktime-test--fmt (worktime--resolve-start 12 0 now))
                   "2026-11-09 12:00")))
  (should (equal (worktime-test--fmt
                  (worktime--resolve-start 22 0 (worktime-test--time "01:00")))
                 "2026-11-08 22:00"))
  (should (equal (worktime-test--fmt
                  (worktime--resolve-start 3 0 (worktime-test--time "23:00")))
                 "2026-11-10 03:00")))

(ert-deftest worktime-test-worked-without-breaks ()
  (let ((day (worktime-test--day "08:00")))
    (should (= (worktime-test--worked day "14:00") 360))
    (should (= (worktime-test--worked day "14:15") 360))
    (should (= (worktime-test--worked day "14:31") 361))
    (should (= (worktime-test--worked day "17:30") 540))
    (should (= (worktime-test--worked day "17:46") 541))))

(ert-deftest worktime-test-worked-with-credited-break ()
  (let ((day (worktime-test--day "08:00" '("09:32" . "09:56"))))
    (should (= (worktime-test--worked day "14:24") 360))
    (should (= (worktime-test--worked day "14:30") 360))
    (should (= (worktime-test--worked day "14:31") 361))))

(ert-deftest worktime-test-worked-with-short-break ()
  (let ((day (worktime-test--day "08:00" '("09:00" . "09:10"))))
    (should (= (worktime-test--worked day "10:00") 110))
    (should (= (worktime-test--worked day "14:10") 360))
    (should (= (worktime-test--worked day "14:40") 360))
    (should (= (worktime-test--worked day "14:41") 361))))

(ert-deftest worktime-test-worked-with-open-break ()
  (let ((day (worktime-test--day "08:00" '("12:00"))))
    (should (= (worktime-test--worked day "12:20") 240))))

(ert-deftest worktime-test-end-time ()
  (should (equal (worktime-test--end (worktime-test--day "08:00")) "16:30"))
  (should (equal (worktime-test--end
                  (worktime-test--day "08:00" '("09:32" . "09:56")))
                 "16:30"))
  (should (equal (worktime-test--end
                  (worktime-test--day "08:00" '("09:32" . "09:56")
                                      '("11:00" . "11:10")))
                 "16:40"))
  (should (equal (worktime-test--end
                  (worktime-test--day "08:00" '("12:00" . "12:45")))
                 "16:45"))
  (worktime-test--with-defaults
    (let ((worktime-hours-per-week 48))
      (should (equal (format-time-string
                      "%R" (worktime--end-time (worktime-test--day "08:00")
                                               (worktime-test--time "08:00")))
                     "18:21")))))

(ert-deftest worktime-test-end-time-moves-during-open-break ()
  (let ((day (worktime-test--day "08:00" '("12:00"))))
    (should (equal (worktime-test--end day "12:20") "16:30"))
    (should (equal (worktime-test--end day "12:40") "16:40"))))

(ert-deftest worktime-test-overdue-p ()
  (worktime-test--with-defaults
    (should-not (worktime--overdue-p (worktime-test--day "08:00")
                                     (worktime-test--time "18:45")))
    (should-not (worktime--overdue-p
                 (worktime-test--day "08:00" '("12:00" . "13:00"))
                 (worktime-test--time "18:45")))
    (should (worktime--overdue-p (worktime-test--day "08:00")
                                 (worktime-test--time "18:46")))
    (should (worktime--overdue-p
             (worktime-test--day "08:00" '("12:00"))
             (worktime-test--time "08:15" "2026-11-10")))))

(ert-deftest worktime-test-custom-break-function ()
  (worktime-test--with-defaults
    (let ((worktime-required-break-function (lambda (_) 30))
          (day (worktime-test--day "08:00")))
      (should (= (worktime--worked-minutes day (worktime-test--time "08:30"))
                 0))
      (should (= (worktime--worked-minutes day (worktime-test--time "16:30"))
                 480)))))

;;;; Mode line

(ert-deftest worktime-test-mode-line-string ()
  (worktime-test--with-defaults
    (should (equal (worktime--mode-line-string
                    (worktime-test--day "08:00") (worktime-test--time "10:00"))
                   " -06:00 16:30"))
    (should (equal (worktime--mode-line-string
                    (worktime-test--day "08:00" '("12:00"))
                    (worktime-test--time "12:20"))
                   " P 00:20 -04:00 16:30"))
    (should (equal (worktime--mode-line-string
                    (worktime-test--day "08:00") (worktime-test--time "17:00"))
                   " +00:30 16:30"))))

(ert-deftest worktime-test-warning-face-at-maximum ()
  (worktime-test--with-defaults
    (let ((day (worktime-test--day "08:00")))
      (should-not (get-text-property
                   1 'face (worktime--mode-line-string
                            day (worktime-test--time "18:44"))))
      (should (eq (get-text-property
                   1 'face (worktime--mode-line-string
                            day (worktime-test--time "18:45")))
                  'worktime-limit-exceeded)))))

(ert-deftest worktime-test-mode-toggles-global-mode-string ()
  (worktime-test--with-file
    (worktime-mode 1)
    (should (memq 'worktime-mode-line-string global-mode-string))
    (worktime-mode 1)
    (should (= 1 (cl-count 'worktime-mode-line-string global-mode-string)))
    (worktime-mode -1)
    (should-not (memq 'worktime-mode-line-string global-mode-string))
    (should-not worktime--timer)))

(ert-deftest worktime-test-warns-about-excessive-workday ()
  (worktime-test--with-file
    (let ((worktime-hours-per-week 44)
          (worktime-days-per-week 4)
          (warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (worktime-mode 1))
      (should warned))))

;;;; Tracking

(ert-deftest worktime-test-full-day-is-stored ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "09:32") (worktime-pause))
    (worktime-test--at (worktime-test--time "09:56") (worktime-resume))
    (worktime-stop (worktime-test--time "16:30"))
    (let ((days (worktime--read-days)))
      (should (= (length days) 1))
      (should (equal (worktime-test--fmt (worktime-day-end (car days)))
                     "2026-11-09 16:30"))
      (should (= (worktime-day-break-minutes (car days)) 30)))
    (should-not worktime--day)
    (should (string-match-p "\"2026-11-09T09:32:00[+-][0-9:]+\""
                            (with-temp-buffer
                              (insert-file-contents worktime-file)
                              (buffer-string))))))

(ert-deftest worktime-test-stop-ends-running-break ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "12:00") (worktime-pause))
    (worktime-stop (worktime-test--time "17:00") (worktime-test--time "12:40"))
    (let ((day (car (worktime--read-days))))
      (should (equal (worktime-test--fmt (worktime-break-end
                                           (car (worktime-day-breaks day))))
                     "2026-11-09 12:40"))
      (should (= (worktime-day-break-minutes day) 40)))))

(ert-deftest worktime-test-mode-resumes-open-day ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-mode -1)
    (setq worktime--day nil)
    (worktime-mode 1)
    (should (time-equal-p (worktime-day-start worktime--day)
                          (worktime-test--time "08:00")))))

(ert-deftest worktime-test-start-rejects-open-day ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (should-error (worktime-start (worktime-test--time "08:00" "2026-11-10"))
                  :type 'user-error)))

(ert-deftest worktime-test-start-stops-open-day ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "12:00") (worktime-pause))
    (worktime-start (worktime-test--time "08:00" "2026-11-10")
                    (worktime-test--time "16:30")
                    (worktime-test--time "12:30"))
    (let ((days (worktime--read-days)))
      (should (= (length days) 2))
      (should (equal (worktime-test--fmt (worktime-day-end (car days)))
                     "2026-11-09 16:30"))
      (should (= (worktime-day-break-minutes (car days)) 30))
      (should-not (worktime-day-end (cadr days))))))

(ert-deftest worktime-test-start-warns-about-short-rest ()
  (worktime-test--with-file
    (let ((warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (worktime-start (worktime-test--time "13:00" "2026-11-08"))
        (worktime-stop (worktime-test--time "22:00" "2026-11-08"))
        (worktime-start (worktime-test--time "08:00"))
        (should (equal (car warnings)
                       (concat "Only 10:00 of rest since 2026-11-08 22:00, "
                               "minimum is 11:00")))
        (should (= (length warnings) 1))
        (worktime-stop (worktime-test--time "16:30"))
        (worktime-start (worktime-test--time "03:30" "2026-11-10"))
        (should (= (length warnings) 1))))))

(ert-deftest worktime-test-hooks-receive-day ()
  (worktime-test--with-file
    (let (started stopped)
      (add-hook 'worktime-start-functions (lambda (day) (setq started day)))
      (add-hook 'worktime-stop-functions (lambda (day) (setq stopped day)))
      (worktime-start (worktime-test--time "08:00"))
      (worktime-stop (worktime-test--time "16:30"))
      (should (time-equal-p (worktime-day-start started)
                            (worktime-test--time "08:00")))
      (should (time-equal-p (worktime-day-end stopped)
                            (worktime-test--time "16:30"))))))

(ert-deftest worktime-test-add-break ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "10:00")
      (worktime-add-break (worktime-test--time "09:32")
                          (worktime-test--time "09:56"))
      (should-error (worktime-add-break (worktime-test--time "09:50")
                                        (worktime-test--time "10:00"))
                    :type 'user-error)
      (should-error (worktime-add-break (worktime-test--time "07:00")
                                        (worktime-test--time "07:30"))
                    :type 'user-error)
      (should-error (worktime-add-break (worktime-test--time "11:00")
                                        (worktime-test--time "10:30"))
                    :type 'user-error))
    (should (= (length (worktime-day-breaks worktime--day)) 1))))

(ert-deftest worktime-test-pause-and-resume-errors ()
  (worktime-test--with-file
    (should-error (worktime-pause) :type 'user-error)
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "10:00")
      (should-error (worktime-resume) :type 'user-error)
      (worktime-pause)
      (should-error (worktime-pause) :type 'user-error))))

(ert-deftest worktime-test-stop-rejects-end-before-break ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "12:30")
      (worktime-add-break (worktime-test--time "12:00")
                          (worktime-test--time "12:30")))
    (should-error (worktime-stop (worktime-test--time "11:00"))
                  :type 'user-error)))


(ert-deftest worktime-test-start-rejects-overlap ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-stop (worktime-test--time "16:30"))
    (should-error (worktime-start (worktime-test--time "08:00"))
                  :type 'user-error)
    (should-error (worktime-start (worktime-test--time "16:00"))
                  :type 'user-error)
    (let ((days (worktime--read-days)))
      (should (= (length days) 1))
      (should (worktime-day-end (car days))))))

(ert-deftest worktime-test-start-keeps-open-day-on-overlap ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (should-error (worktime-start (worktime-test--time "16:00")
                                  (worktime-test--time "16:30"))
                  :type 'user-error)
    (should-not (worktime-day-end (car (worktime--read-days))))))

(ert-deftest worktime-test-days-per-week-must-be-positive ()
  (let ((type (widget-convert (get 'worktime-days-per-week 'custom-type))))
    (should (widget-apply type :match 5))
    (should-not (widget-apply type :match 0))))

(ert-deftest worktime-test-mode-survives-corrupt-file ()
  (worktime-test--with-file
    (with-temp-file worktime-file
      (insert "((:start"))
    (let ((warned nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _) (setq warned t))))
        (worktime-mode 1))
      (should warned)
      (should worktime-mode)
      (should-not worktime--day))
    (should-error (worktime-pause))))

(defun worktime-test--stop-args (day now)
  "Call `worktime--read-stop-args', accepting every default.
Return the arguments and the prompts shown."
  (let ((prompts nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt _initial _history default)
                 (push prompt prompts)
                 default)))
      (list (mapcar (lambda (time) (and time (worktime-test--fmt time)))
                    (worktime--read-stop-args day now))
            (nreverse prompts)))))

(ert-deftest worktime-test-read-stop-args-offers-now ()
  (worktime-test--with-file
    (pcase-let ((`(,args ,prompts)
                 (worktime-test--stop-args
                  (worktime-test--day "08:00" '("12:00"))
                  (worktime-test--time "12:20"))))
      (should (equal args '("2026-11-09 12:20" "2026-11-09 12:20")))
      (should (= (length prompts) 2))
      (should (string-match-p "(default 12:20)" (car prompts))))))

(ert-deftest worktime-test-read-stop-args-offers-calculated-ends ()
  (worktime-test--with-file
    (pcase-let ((`(,args ,_)
                 (worktime-test--stop-args
                  (worktime-test--day "08:00" '("12:00"))
                  (worktime-test--time "08:15" "2026-11-10"))))
      (should (equal args '("2026-11-09 16:30" "2026-11-09 12:30"))))
    (pcase-let ((`(,args ,prompts)
                 (worktime-test--stop-args
                  (worktime-test--day "08:00")
                  (worktime-test--time "08:15" "2026-11-10"))))
      (should (equal args '("2026-11-09 16:30" nil)))
      (should (= (length prompts) 1)))))


(ert-deftest worktime-test-add-break-rejects-future ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "10:00")
      (should-error (worktime-add-break (worktime-test--time "10:30")
                                        (worktime-test--time "10:45"))
                    :type 'user-error)
      (let ((start (worktime--time-after 7 30 (worktime-test--time "08:00"))))
        (should-error (worktime-add-break start (time-add start (* 15 60)))
                      :type 'user-error)))
    (should-not (worktime-day-breaks (worktime--open-day)))))

(ert-deftest worktime-test-stop-rejects-future ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "16:30")
      (should-error (worktime-stop (worktime--time-after
                                    7 30 (worktime-test--time "08:00")))
                    :type 'user-error))
    (should (worktime--open-day))))

(ert-deftest worktime-test-start-rejects-future-previous-end ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "08:00"))
    (worktime-test--at (worktime-test--time "07:00" "2026-11-10")
      (let ((err (should-error
                  (worktime-start (worktime-test--time "09:00" "2026-11-10")
                                  (worktime-test--time "08:00" "2026-11-10"))
                  :type 'user-error)))
        (should (equal (cadr err)
                       "Working day ends in the future"))))
    (should (= (length (worktime--read-days)) 1))
    (should (worktime--open-day))))

(ert-deftest worktime-test-night-shift ()
  (worktime-test--with-file
    (worktime-start (worktime-test--time "22:00"))
    (worktime-test--at (worktime-test--time "06:30" "2026-11-10")
      (worktime-stop (worktime--time-after 6 30 (worktime-test--time "22:00"))))
    (let ((day (car (worktime--read-days))))
      (should (equal (worktime-test--fmt (worktime-day-end day))
                     "2026-11-10 06:30"))
      (should (= (worktime-day-break-minutes day) 30)))))

(ert-deftest worktime-test-timedot-file ()
  (worktime-test--with-file
    (let ((worktime-timedot-file (make-temp-file "worktime-test-" nil
                                                 ".timedot")))
      (unwind-protect
          (progn
            (worktime-start (worktime-test--time "08:00"))
            (worktime-test--at (worktime-test--time "12:00") (worktime-pause))
            (worktime-test--at (worktime-test--time "12:45") (worktime-resume))
            (worktime-stop (worktime-test--time "17:00"))
            (worktime-start (worktime-test--time "08:00" "2026-11-10"))
            (worktime-stop (worktime-test--time "15:41" "2026-11-10"))
            (worktime-start (worktime-test--time "08:00" "2026-11-11"))
            (should (equal (with-temp-buffer
                             (insert-file-contents worktime-timedot-file)
                             (buffer-substring (line-beginning-position 2)
                                               (point-max)))
                           (concat "\n2026-11-09\nwork   495m\nbreak  45m\n"
                                   "\n2026-11-10\nwork   431m\nbreak  30m\n"))))
        (delete-file worktime-timedot-file)))))

(ert-deftest worktime-test-no-timedot-file-by-default ()
  (worktime-test--with-file
    (let ((default-directory (make-temp-file "worktime-test-" t)))
      (unwind-protect
          (progn
            (worktime-start (worktime-test--time "08:00"))
            (worktime-stop (worktime-test--time "16:30"))
            (should-not (directory-files default-directory nil
                                         directory-files-no-dot-files-regexp)))
        (delete-directory default-directory t)))))

;;; worktime-test.el ends here
