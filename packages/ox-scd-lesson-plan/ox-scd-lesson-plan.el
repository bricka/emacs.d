;;; ox-scd-lesson-plan --- Export an Org document as an SCD lesson plan  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ox)
(require 'ox-html)

(defun ox-scd-lesson-plan-headline-translator (headline contents info)
  "Transcode a HEADLINE into the table.  CONTENTS has the content, INFO is a plist with contextual info."
  (let* ((headline-text (org-export-data (org-element-property :title headline) info))
         (level (org-element-property :level headline))
         (start-time (plist-get info 'time-so-far))
         (raw-effort (org-element-property :EFFORT headline))
         (effort (when raw-effort (org-duration-to-minutes raw-effort))))
    (cond
     ((= level 1)
      (when effort
        (plist-put info 'time-so-far (+ start-time effort)))
      (format "<tr class=\"table-section\"><th colspan=\"5\">%s</th>%s</tr>" headline-text (or contents "")))
     ((= level 2)
      (let* ((end-time (+ start-time (or effort 0))))
        (plist-put info 'time-so-far end-time)
        (format "<tr><td>%s&nbsp;-&nbsp;%s</td><td>%s</td>%s</tr>"
                (org-duration-from-minutes start-time)
                (org-duration-from-minutes end-time)
                headline-text
                (or contents "")))
        )
     (t (format "<td>%s</td>" (or contents "")))
     )
    )
  )


(defun ox-scd-lesson-plan-template (contents info)
  "Wrap the CONTENTS in the top-level template.  INFO is a plist with contextual info."
  (let ((title (org-export-data (plist-get info :title) info))
        (date (org-export-data (plist-get info :date) info)))
    (format "
<html>
<head>
  %s
  <link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.2.2/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-Zenh87qX5JnK2Jl0vWa8Ck2rdkQ2Bzep5IDxbcnCeuOxjzrPF/et3URy9Bv1WTRi\" crossorigin=\"anonymous\">
</head>
<body>
<div class=\"container\">
%s
%s
%s
</div>
</body>
</html>"
            (if title (format "<title>%s</title>" title) "")
            (if title (format "<h1>%s</h1>" title) "")
            (if date (format "<h2>%s</h2>" date) "")
            contents)
    ))

(defun ox-scd-lesson-plan-inner-template (contents info)
  "Wrap the CONTENTS in a table.  INFO is a plist with contextual info."
  (let ((body-only-p (memq 'body-only (plist-get info :export-options))))
    (format "%s<table class=\"table table-bordered table-striped\"><thead><tr><th>Time</th><th>What</th><th>Class Arrangement</th><th>Details</th><th>Music</th></tr></thead><tbody>%s</tbody></table>%s"
            (if body-only-p "<div class=\"container\">" "")
            contents
            (if body-only-p "</div>" ""))
    )
  )

(org-export-define-derived-backend 'scd-lesson-plan 'html
  :translate-alist '((headline . ox-scd-lesson-plan-headline-translator)
                     (inner-template . ox-scd-lesson-plan-inner-template)
                     (template . ox-scd-lesson-plan-template))
  )


(defun org-export-to-lesson-plan (&optional async subtreep visible-only body-only)
  "Export this file as an SCD lesson plan."
  (interactive)
  (setq-local org-html-head "<link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.2.2/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-Zenh87qX5JnK2Jl0vWa8Ck2rdkQ2Bzep5IDxbcnCeuOxjzrPF/et3URy9Bv1WTRi\" crossorigin=\"anonymous\">")
  (let ((outfile (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'scd-lesson-plan outfile
      async subtreep visible-only body-only '(time-so-far 0))))

(provide 'ox-scd-lesson-plan)
;;; ox-scd-lesson-plan.el ends here
