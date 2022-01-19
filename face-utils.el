;;;  face-utils.el --- Helper functions for faces  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar face-hooks nil "Hooks that should be run to set faces correctly.")

(defun my/set-face-attribute (face frame &rest args)
  "Like `set-face-attribute', but also add to 'face-hooks so it will be re-run as necessary.

FACE, FRAME, and ARGS as in `set-face-attribute'."
  (let ((hook (lambda () (apply 'set-face-attribute face frame args))))
    (add-hook 'face-hooks hook)
    (funcall hook)
    )
  )

(provide 'face-utils)
;;; face-utils.el ends here
