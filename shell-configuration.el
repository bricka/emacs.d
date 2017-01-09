(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  "Kill term buffers when they exit."
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun my-term-use-utf8 ()
  "Set the term's coding system to UTF-8."
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(defun visit-term-projectile-root ()
  "Visit a terminal buffer, creating it in the projectile root if it doesn't exist."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (visit-term-buffer)))
