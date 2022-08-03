;;; pem-mode.el --- Helpers for PEM files

;;; Commentary:

;;; Code:

(require 'x509-mode)

(defconst pem--font-lock-keywords
  '(
    ("^-----.*-----$" . font-lock-comment-face)
    ))

;;;###autoload
(define-derived-mode pem-mode prog-mode "PEM"
  (setq font-lock-defaults '(pem--font-lock-keywords))
  )

(general-define-key
 :states 'normal
 :keymaps 'pem-mode-map
 "RET" #'x509-dwim
 )

(add-to-list 'auto-mode-alist '("\\.crt" . pem-mode))

(provide 'pem-mode)
;;; pem-mode.el ends here
