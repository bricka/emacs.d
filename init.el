;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Disable Menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Straightforward config
(setq scroll-step 1)
(setq-default indent-tabs-mode nil)
(setq delete-old-versions t)
(setq inhibit-startup-screen t)
(setq-default require-final-newline t)
(setq-default make-backup-files nil)

(cond ((eq system-type 'darwin) (set-face-attribute 'default nil :font "Menlo 15"))
      (t (set-face-attribute 'default nil :font "DejaVu Sans Mono 14")))

;; Indentation
(defun my/set-indentation (indentation)
  "Set the indentation level to INDENTATION."
  (setq-default standard-indent indentation
                tab-width indentation))

(my/set-indentation 4)

;; Parens
(show-paren-mode 1)
(electric-pair-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; PATH from Shell

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Evil
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :config

  (evil-mode 1)
  (evil-ex-define-cmd "ls" 'helm-mini)
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (evil-add-hjkl-bindings messages-buffer-mode-map 'emacs)

  (use-package evil-magit
    :ensure t)

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>"))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)))

;; Git Configuration
(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))) ; Disable VC for Git

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

;; Powerline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

;; Theme
(load-theme 'spacemacs-dark 1)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-use-git-grep t)

  (use-package helm-projectile
    :ensure t))

;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Web Mode
(defun my/web-mode-is-js ()
  "Determine if the current buffer is using a JS web-mode content type."
  (member web-mode-content-type '("js" "jsx")))

(defun enable-tern-mode-for-js ()
  "Enable tern-mode if this web-mode content type is a form of JS."
  (if (my/web-mode-is-js) (tern-mode)))

(defun my/create-web-mode-js-syntax-table ()
  "Create a syntax table for web mode in JS."
  (let ((st (make-syntax-table web-mode-syntax-table))
        (use-hook (lambda () (if (my/web-mode-is-js) (set-syntax-table web-mode-js-syntax-table)))))
    (progn
      (modify-syntax-entry ?` "\"" st)
      (modify-syntax-entry ?' "\"" st)
      (defvar web-mode-js-syntax-table st "Syntax table for web-mode when parsing JS")
      (add-hook 'web-mode-hook use-hook))))

(use-package web-mode
  :ensure t

  :config
  (my/create-web-mode-js-syntax-table)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook 'enable-tern-mode-for-js))

;; JSON
(use-package json-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode)))

;; Which Key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :ensure t

  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil)

  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))

;; Rainbow Mode
(use-package rainbow-mode
  :ensure t

  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

;; ENSIME
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; Flycheck
(defun my/use-eslint-from-node-modules ()
  "Configure Flycheck to use eslint from node_modules if present."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-eslint (executable-find "eslint"))
         (local-eslint (expand-file-name "node_modules/.bin/eslint"
                                         root))
         (eslint (if (file-executable-p local-eslint)
                     local-eslint
                   global-eslint)))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

;; Shell
(load-file "~/.emacs.d/shell-configuration.el")

;; Dired

(setq-default dired-omit-mode t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\...+$"))

(defun dired-open-current-directory ()
  "Open dired in the directory of this file."
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

;; Latex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)

(add-hook 'latex-mode-hook 'TeX-source-correlate-mode)

;; LESS

(use-package less-css-mode
  :ensure t)

;; Keys
(defun set-group-string (prefix title)
  "Set the which-key string for LEADER PREFIX to TITLE."
  (which-key-add-key-based-replacements
    (concat evil-leader/leader " " prefix) title))

;; Top-Level Keys
(evil-leader/set-key
  "'" 'visit-term-buffer
  "d" 'dired-open-current-directory)

;; Buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(set-group-string "b" "Buffers")
(evil-leader/set-key
  "bb" 'helm-mini
  "bd" 'kill-this-buffer
  "bD" 'kill-other-buffers)

;; Errors
(set-group-string "e" "Errors")
(evil-leader/set-key
  "el" 'flycheck-list-errors
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  "ev" 'flycheck-verify-setup
  )

;; File Keys
(defun visit-emacs-init ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; These are taken from Steve Yegge's .emacs:
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
   (progn
     (rename-file filename new-name 1)
     (rename-buffer new-name)
     (set-visited-file-name new-name)
     (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Move both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
    (if (string-match dir "\\(?:/\\|\\\\)$")
        (substring dir 0 -1) dir))
   (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
  (progn
    (copy-file filename newname 1)
    (delete-file filename)
    (set-visited-file-name newname)
    (set-buffer-modified-p nil)
    t))))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (cond ((vc-backend filename) (vc-delete-file filename))
            (t (progn
                 (delete-file filename)
                 (message "Deleted file %s" filename)
                 (kill-buffer)))))))

(set-group-string "f" "Files")
(evil-leader/set-key
  "fd" 'delete-file-and-buffer
  "fe" 'visit-emacs-init
  "fm" 'move-buffer-file
  "fr" 'rename-buffer-and-buffer
  )

;; Git Keys
(set-group-string "g" "Git")
(evil-leader/set-key
  "gb" 'magit-branch-popup
  "gc" 'magit-commit-popup
  "gd" 'magit-diff-popup
  "gp" 'magit-push-popup
  "gs" 'magit-status)

(set-group-string "gh" "Hunks")
(evil-leader/set-key
  "ghn" 'git-gutter:next-hunk
  "ghp" 'git-gutter:previous-hunk)

;; Help Keys
(set-group-string "h" "Help")
(evil-leader/set-key
  "ha" 'helm-apropos
  "hf" 'describe-function
  "hk" 'describe-key
  "hm" 'describe-mode
  "hv" 'describe-variable)

;; Project Keys
(set-group-string "p" "Project")
(evil-leader/set-key
  "p'" 'visit-term-projectile-root
  "pi" 'projectile-invalidate-cache
  "pf" 'helm-projectile-find-file
  "pl" 'helm-projectile-switch-project)

;; Search Keys
(set-group-string "s" "Search")
(evil-leader/set-key
  "sp" 'helm-projectile-grep
  )

;; Window Keys
(set-group-string "w" "Window")
(evil-leader/set-key
  "w-" 'split-window-below
  "w/" 'split-window-right
  "w=" 'balance-windows)

;;; MAJOR MODE KEYS
(load-file "~/.emacs.d/major-mode.el")

;; Mode Line
;; We put this last so that all modes are already loaded.
(use-package delight
  :ensure t
  :config
  (delight '((rainbow-mode nil t)
             (helm-mode nil t)
             (git-gutter-mode nil t)
             (which-key-mode nil t)
             (evil-commentary-mode nil t)
             (company-mode nil t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (delight rainbow-delimiters evil-surround git-gutter-fringe diff-hl rainbow-mode less-css-mode web-mode json-mode jsdon-mode spaceline-config evil-magit use-package helm monokai-theme moe-theme color-theme-sanityinc-tomorrow zenburn-theme spaceline powerline flx-ido projectile magit evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
