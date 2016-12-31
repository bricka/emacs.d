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

;; Line Number
(global-linum-mode 1)

;; Evil
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "ls" 'helm-mini)
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (use-package evil-magit
    :ensure t)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")))

;; Magit Configuration
(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))) ; Disable VC for Git

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
  (use-package helm-projectile
    :ensure t))

;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Web Mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode)))

;; JSON
(use-package json-mode
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; ENSIME
(use-package ensime
  :ensure t
  :pin melpa-stable)

;; Top-Level Keys
(evil-leader/set-key
  "'" 'ansi-term
  "b" 'helm-mini)

;; File Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " f") "Files")
(evil-leader/set-key
  "fc" 'dired-do-copy
  "fm" 'dired-do-rename
  )

;; Git Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " g") "Git")
(evil-leader/set-key
  "gc" 'magit-commit
  "gs" 'magit-status)

;; Help Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " h") "Help")

(evil-leader/set-key
  "ha" 'apropos
  "hm" 'describe-mode
  "hv" 'describe-variable)

;; Project Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " p") "Project")

(evil-leader/set-key
  "pf" 'helm-projectile-find-file
  "pl" 'helm-projectile-switch-project)

;; Search Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " s") "Search")

(evil-leader/set-key
  "sp" 'helm-projectile-grep
  )

;; Window Keys
(which-key-add-key-based-replacements
  (concat evil-leader/leader " w") "Window")

(evil-leader/set-key
  "w-" 'split-window-below
  "w/" 'split-window-right)

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
    (web-mode json-mode jsdon-mode spaceline-config evil-magit use-package helm monokai-theme moe-theme color-theme-sanityinc-tomorrow zenburn-theme spaceline powerline flx-ido projectile magit evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
