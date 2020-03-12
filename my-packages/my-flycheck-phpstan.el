;;; my-flycheck-phpstan.el --- My Fork of flycheck-phpstan  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.3") (flycheck "26") (phpstan "0.2.1"))

;;; Code:
(require 'flycheck)
(require 'phpstan)

;; Usually it is defined dynamically by flycheck
(defvar flycheck-phpstan-executable)

(defun flycheck-phpstan--enabled-and-set-variable ()
  "Return path to phpstan configure file, and set buffer execute in side effect."
  (let ((enabled (or phpstan-working-dir (phpstan-get-config-file))))
    (prog1 enabled
      (when (and phpstan-flycheck-auto-set-executable
                 (not (and (boundp 'flycheck-phpstan-executable)
                           (symbol-value 'flycheck-phpstan-executable)))
                 (or (eq 'docker phpstan-executable)
                     (and (consp phpstan-executable)
                          (stringp (car phpstan-executable))
                          (listp (cdr phpstan-executable)))))
        (set (make-local-variable 'flycheck-phpstan-executable)
               (car phpstan-executable))))))

(flycheck-define-checker phpstan
  "PHP static analyzer based on PHPStan."
  :command ("php" (eval (phpstan-get-command-args)) source-original)
  :working-directory (lambda (_) (phpstan-get-working-dir))
  :enabled (lambda () (flycheck-phpstan--enabled-and-set-variable))
  :predicate flycheck-buffer-saved-p
  :error-patterns
  ((error line-start (1+ (not (any ":"))) ":" line ":" (message) line-end))
  :modes (php-mode phps-mode))

(add-to-list 'flycheck-checkers 'phpstan t)
(flycheck-add-next-checker 'php 'phpstan)

(provide 'my-flycheck-phpstan)
;;; my-flycheck-phpstan.el ends here
