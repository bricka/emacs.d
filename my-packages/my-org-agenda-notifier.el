;;; my-org-agenda-notifier.el --- Customizable org-agenda notifications -*- lexical-binding: t -*-

;; Package-Requires: ((alert "1.2") (async "1.9.3") (dash "2.13.0") (dash-functional "1.2.0") (emacs "24.4"))

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'alert)
(require 'async)
(require 'org-agenda)
(require 'cl-lib)


(defgroup my-org-agenda-notifier nil
  "my-org-agenda-notifier customization options"
  :group 'org)

(defcustom my-org-agenda-notifier-alert-time '(0 10)
  "Time in minutes to get a notification about upcomming event.
Cannot be less than 1."
  :package-version '(my-org-agenda-notifier . "0.1.0")
  :group 'my-org-agenda-notifier
  :type '(repeat integer))

(defcustom my-org-agenda-notifier-alert-times-property "WILD_NOTIFIER_NOTIFY_BEFORE"
  "Use this property in your agenda files to add additional notifications \
to an event."
  :package-version '(my-org-agenda-notifier . "0.1.0")
  :group 'my-org-agenda-notifier
  :type 'string)

(defcustom my-org-agenda-notifier-notification-title "Agenda"
  "Notifications title."
  :package-version '(my-org-agenda-notifier . "0.1.0")
  :group 'my-org-agenda-notifier
  :type 'string)

(defcustom my-org-agenda-notifier-keyword-blacklist nil
  "Never receive notifications for these keywords."
  :package-version '(my-org-agenda-notifier . "0.2.2")
  :group 'my-org-agenda-notifier
  :type '(repeat string))

(defcustom my-org-agenda-notifier--alert-severity 'medium
  "Severity of the alert.
options: 'high 'medium 'low"
  :package-version '(my-org-agenda-notifier . "0.3.1")
  :group 'my-org-agenda-notifier
  :type 'symbol
  :options '(high medium low))

(defvar my-org-agenda-notifier--day-wide-events nil
  "If truthy, notifies about day-wide events.")

(defvar my-org-agenda-notifier--timer nil
  "Timer value.")

(defvar my-org-agenda-notifier--agenda-buffer-name "*org wild notifier affairs*"
  "A name for temporary 'org-agenda' buffer.")

(defun my-org-agenda-notifier--time= (&rest list)
  "Compare timestamps.
Comparison is performed by converted each element of LIST onto string
in order to ignore seconds."
  (->> list
       (--map (format-time-string "%d:%H:%M" it))
       (-uniq)
       (length)
       (= 1)))

(defun my-org-agenda-notifier--today ()
  "Get the timestamp for the beginning of current day."
  (apply 'encode-time
         (append '(0 0 0) (nthcdr 3 (decode-time (current-time))))))

(defun my-org-agenda-notifier--always-notify-p (event)
  "Check that notification for the EVENT should be done regardless of time.
For now, the only case that handled is day-wide events."
  (when my-org-agenda-notifier--day-wide-events
    (let ((today (my-org-agenda-notifier--today)))
      ;; SPIKE: Org timestamps without "time" section are shorter than
      ;; 16 characters.
      (--any-p (and (<= (length (car it)) 16) (equal today (cdr it)))
               (cadr (assoc 'times event))))))

(defun my-org-agenda-notifier--timestamp-within-interval-p (timestamp interval)
  "Check whether TIMESTAMP is within notification INTERVAL."
  (my-org-agenda-notifier--time=
   (time-add (current-time) (seconds-to-time (* 60 interval)))
   timestamp))

(defun my-org-agenda-notifier--notifications (event)
  "Get notifications for given EVENT.
Returns a list of notification intervals."
  (if (my-org-agenda-notifier--always-notify-p event)
      '(-1)

    (->> `(,(cadr (assoc 'times event)) ,(cdr (assoc 'intervals event)))
         (apply '-table-flat (lambda (ts int) `(,(cdr ts) ,int)))
         (--filter (apply 'my-org-agenda-notifier--timestamp-within-interval-p it))
         (-map 'cadr))))

(defun my-org-agenda-notifier--time-left (seconds)
  "Human-friendly representation for SECONDS."
  (-> seconds
       (pcase
         ((pred (>= 0)) "today")
         ((pred (>= 3600)) "in %M")
         (_ "in %H %M"))

       (format-seconds seconds)))

(defun my-org-agenda-notifier--notification-text (interval event)
  "For given INTERVAL and EVENT get notification wording."
  (format "%s %s"
          (cdr (assoc 'title event))
          (my-org-agenda-notifier--time-left (* 60 interval))))

(defun my-org-agenda-notifier--check-event (event)
  "Get notifications for given EVENT.
Returns a list of notification messages"
  (->> (my-org-agenda-notifier--notifications event)
       (--map (my-org-agenda-notifier--notification-text it event))))

(defun my-org-agenda-notifier--get-tags (marker)
  "Retrieve tags of MARKER."
  (-> (org-entry-get marker "TAGS")
      (or "")
      (org-split-string  ":")))

(defun my-org-agenda-notifier--blacklist-predicates ()
  (->> `([,my-org-agenda-notifier-keyword-blacklist
          (lambda (it)
            (-contains-p my-org-agenda-notifier-keyword-blacklist
                         (org-entry-get it "TODO")))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun my-org-agenda-notifier--apply-blacklist (markers)
  "Apply blacklist to MARKERS."
  (-if-let (blacklist-predicates (my-org-agenda-notifier--blacklist-predicates))
      (-> (apply '-orfn blacklist-predicates)
          (-remove markers))
    markers))

(defun my-org-agenda-notifier--retrieve-events ()
  "Get events from agenda view."
  (let ((agenda-files (-filter 'file-exists-p org-agenda-files))
        ;; Some package managers manipulate `load-path` variable.
        (my-load-path load-path))
    (lambda ()
      (let ((org-agenda-use-time-grid nil)
            (org-agenda-compact-blocks t))
        (setf org-agenda-files agenda-files)
        (setf load-path my-load-path)

        (package-initialize)
        (require 'my-org-agenda-notifier)

        (org-agenda-list 2
                         (org-read-date nil nil "today"))

        (->> (org-split-string (buffer-string) "\n")
             (--map (plist-get
                     (org-fix-agenda-info (text-properties-at 0 it))
                     'org-marker))
             (-non-nil)
             (my-org-agenda-notifier--apply-blacklist)
             (-map 'my-org-agenda-notifier--gather-info))))))

(defun my-org-agenda-notifier--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  (alert event-msg :title my-org-agenda-notifier-notification-title :severity my-org-agenda-notifier--alert-severity))

(defun my-org-agenda-notifier--extract-time (marker)
  "Extract timestamps from MARKER.
Timestamps are extracted as cons cells.  car holds org-formatted
string, cdr holds time in list-of-integer format."
  (-non-nil
   (--map
    (let ((org-timestamp (org-entry-get marker it)))
      (and org-timestamp
           (cons org-timestamp
                 (apply 'encode-time (org-parse-time-string org-timestamp)))))
    '("DEADLINE" "SCHEDULED" "TIMESTAMP"))))

(defun my-org-agenda-notifier--extract-title (marker)
  "Extract event title from MARKER.
MARKER acts like the event's identifier."
  (org-with-point-at marker
    (-let (((_lvl _reduced-lvl _todo _priority title _tags)
            (org-heading-components)))
      title)))

(defun my-org-agenda-notifier--extract-notication-intervals (marker)
  "Extract notification intervals from the event's properties.
MARKER acts like the event's identifier.  Resulting list also contains
standard notification interval (`my-org-agenda-notifier-alert-time')."
  `(,@my-org-agenda-notifier-alert-time
    ,@(-map 'string-to-number
           (org-entry-get-multivalued-property
            marker
            my-org-agenda-notifier-alert-times-property))))

(defun my-org-agenda-notifier--gather-info (marker)
  "Collect information about an event.
MARKER acts like event's identifier."
  `((times . (,(my-org-agenda-notifier--extract-time marker)))
    (title . ,(my-org-agenda-notifier--extract-title marker))
    (intervals . ,(my-org-agenda-notifier--extract-notication-intervals marker))))

(defun my-org-agenda-notifier--stop ()
  "Stops the notification timer."
  (-some-> my-org-agenda-notifier--timer (cancel-timer)))

(defun my-org-agenda-notifier--start ()
  "Start the notification timer.  Cancel old one, if any.
Timer is scheduled on the beginning of every minute, so for
smoother experience this function also runs a check without timer."
  (my-org-agenda-notifier--stop)

  (let ((my-org-agenda-notifier--day-wide-events t))
    (my-org-agenda-notifier-check))

  (--> (format-time-string "%H:%M" (time-add (current-time) 60))
       (run-at-time it 60 'my-org-agenda-notifier-check)
       (setf my-org-agenda-notifier--timer it)))

;;;###autoload
(defun my-org-agenda-notifier-check ()
  "Parse agenda view and notify about upcomming events."
  (interactive)

  (async-start
   (my-org-agenda-notifier--retrieve-events)
   (lambda (events)
     (-each
         (->> events
              (-map 'my-org-agenda-notifier--check-event)
              (-flatten)
              (-uniq))
       'my-org-agenda-notifier--notify))))

;;;###autoload
(define-minor-mode my-org-agenda-notifier-mode
  "Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed."
  :global
  :lighter "Org Wild Notifier"
  (if my-org-agenda-notifier-mode
      (my-org-agenda-notifier--start)
    (my-org-agenda-notifier--stop)))

(provide 'my-org-agenda-notifier)

;;; my-org-agenda-notifier.el ends here
