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

(defun my/open-local-terminal (name)
  "Visit terminal on the local machine at `default-directory' with name NAME."
  (let ((bufname (concat "*" name "*")))
    (if (get-buffer bufname)
        (switch-to-buffer-other-window bufname)
      (split-window-sensibly (selected-window))
      (other-window 1)
      (ansi-term "/bin/zsh" name)
      )
    )
  )

;; Inspired by https://emacs.stackexchange.com/a/31507
(defun my/open-tramp-terminal (name)
  "Visit terminal on remote machine at `default-directory' with name NAME."
  (let ((tstruct (tramp-dissect-file-name default-directory))
        (bufname (concat "*" name "*")))
    (split-window-sensibly (selected-window))
    (other-window 1)
    (ansi-term "/bin/zsh" name)
    (cond
     ((equal (tramp-file-name-method tstruct) "ssh")
      (process-send-string
       bufname
       (format "ssh -t %s@%s 'cd %s; clear; /bin/bash'\n"
               (tramp-file-name-user tstruct)
               (tramp-file-name-host tstruct)
               (tramp-file-name-localname tstruct)
               )
       )
      )
     (t (error "Cannot open terminal when using method %s" (tramp-file-name-method tstruct)))
     )))

(defun my/open-terminal (&optional name)
  "Open an ANSI terminal at `default-directory'.  The name of the buffer is based on NAME."
  (interactive)
  (unless name (setq name "ansi-term"))
  (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p default-directory))
      (my/open-tramp-terminal name)
    (my/open-local-terminal name)
    )
  )

(defun visit-term-projectile-root ()
  "Visit a terminal buffer, creating it in the projectile root if it doesn't exist."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (my/open-terminal (projectile-project-name))))
