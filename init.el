;; Enable MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Disable Menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
(setq vc-handled-backends (delq 'Git vc-handled-backends)) ; Disable VC for Git

;; Powerline
(require 'spaceline-config)
(spaceline-spacemacs-theme)

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (web-mode json-mode jsdon-mode spaceline-config evil-magit use-package helm monokai-theme moe-theme color-theme-sanityinc-tomorrow zenburn-theme spaceline powerline flx-ido projectile magit evil)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
