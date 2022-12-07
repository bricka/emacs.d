;;; init.el --- Alex Brick's Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

(setq load-path
      (append load-path
              (directory-files (concat (expand-file-name user-emacs-directory) "packages") t directory-files-no-dot-files-regexp t)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

(add-to-list 'straight-profiles '(local . "local.el"))

(straight-use-package 'use-package)

(setq straight-host-usernames
      '((github . "bricka")
        (gitlab . "bricka")))

;; Disable Menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Straightforward config
(setq-default
 indent-tabs-mode nil
 require-final-newline t
 make-backup-files nil
 )
(setq
 scroll-step 1
 delete-old-versions t
 inhibit-startup-screen t
 ring-bell-function 'ignore
 custom-file "/dev/null"
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024) ;; 1mb
 clean-buffer-list-delay-general 2
 confirm-kill-emacs #'yes-or-no-p
 native-comp-async-report-warnings-errors 'silent
 )

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Helper functions for handling font faces
(defvar face-hooks nil "Hooks that should be run to set faces correctly.")

(defun my/set-face-attribute (face frame &rest args)
  "Like `set-face-attribute', but also add to `face-hooks'.

This means that it will be re-run as necessary.

FACE, FRAME, and ARGS as in `set-face-attribute'."
  (let ((hook (lambda () (apply 'set-face-attribute face frame args))))
    (add-hook 'face-hooks hook)
    (funcall hook)
    )
  )

;; Scratch
(setq initial-scratch-message nil)
(when (and (executable-find "cowsay")
           (executable-find "fortune"))
  (setq
   initial-scratch-message
   (string-join
    (seq-map
     (lambda (l)
       (string-trim-right (concat ";; " l)))
     (split-string
      (shell-command-to-string "fortune | cowsay")
      "\n")
     )
    "\n")))

(defun my/configure-fonts-for-frame (frame)
  "Configure fonts for FRAME."
  (when frame
    (my/set-face-attribute 'default frame :font "DejaVu Sans Mono 15")
    (my/set-face-attribute 'variable-pitch frame :font "DejaVu Serif 15")
    ))

(my/configure-fonts-for-frame (selected-frame))
(add-hook 'after-make-frame-functions #'my/configure-fonts-for-frame)

;; Indentation
(defun my/set-indentation (indentation)
  "Set the indentation level to INDENTATION."
  (setq-default standard-indent indentation
                tab-width indentation
                web-mode-code-indent-offset indentation
                typescript-indent-level indentation
                js-indent-level indentation
                plantuml-indent-level indentation
                )
  )

(my/set-indentation 2)

(defun my/disable-auto-fill-mode ()
  "Ideally used as a hook to disable 'auto-fill-mode'."
  (auto-fill-mode -1)
  )

(defun my/disable-auto-save-mode ()
  "Ideally used as a hook to disable 'auto-save-mode'."
  (auto-save-mode -1)
  )

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

;; Parens
(show-paren-mode 1)
(electric-pair-mode 1)

;; Key Bindings
(defconst my-leader-key "SPC"
  "Normal leader key for keybindings."
  )

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-define-key
   :prefix my-leader-key
   :states 'normal
   "" nil
   "'" #'my/open-terminal
   "d" #'dired-open-current-directory

   "b" '(:ignore t :wk "Buffers")
   "bd" #'kill-this-buffer
   "bD" #'my/kill-other-buffers
   "br" #'revert-buffer

   "e" '(:ignore t :wk "Errors")

   "f" '(:ignore t :wk "Files")

   "g" '(:ignore t :wk "Git")
   "gh" '(:ignore t :wk "Hunks")

   "h" '(:ignore t :wk "Help")
   "hc" #'describe-char
   "hm" #'describe-mode

   "m" '(:ignore t :wk "Major")

   "p" '(:ignore t :wk "Project")

   "s" '(:ignore t :wk "Search")

   "w" '(:ignore t :wk "Window")
   )

  (general-define-key
   :states 'normal
   :keymaps 'message-mode-map
   "q" #'quit-window)
  )

(use-package discover-my-major
  :general
  (:prefix my-leader-key
   :states 'normal
   "hM" #'discover-my-major
   )
  )

(use-package helpful
  :general
  (:prefix my-leader-key
   :states 'normal
   "hf" #'helpful-function
   "hk" #'helpful-key
   "hv" #'helpful-variable
   )
  ("C-h f" #'helpful-function
   "C-h k" #'helpful-key
   "C-h v" #'helpful-variable
   )
  :config
  (general-define-key
   :keymaps 'helpful-mode-map
   :states 'normal
   "q" #'quit-window)
  )


;; Mode Line
(use-package blackout
  :config
  (eval-after-load 'auto-revert
    (blackout 'auto-revert-mode)
    )
  (blackout 'eldoc-mode)
  (blackout 'undo-tree-mode)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; PATH from Shell

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-arguments nil)
    (add-to-list 'exec-path-from-shell-variables "EMACS_LOCAL_CONFIG_PATH")
    (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
    (exec-path-from-shell-initialize)
    )
  )

(use-package all-the-icons
  :config
  (add-to-list 'all-the-icons-extension-icon-alist '("ics" all-the-icons-octicon "calendar" :face all-the-icons-silver))
  (add-to-list 'all-the-icons-extension-icon-alist '("deb" all-the-icons-octicon "package" :face all-the-icons-silver))
  (add-to-list 'all-the-icons-regexp-icon-alist '("^gradlew\\(.bat\\)?$" all-the-icons-alltheicon "terminal" :height 1.0 :v-adjust 0.0 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-regexp-icon-alist '("^.gitlab-ci.yml$" all-the-icons-fileicon "gitlab" :height 1.0 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-regexp-icon-alist '("^openapi.ya?ml$" all-the-icons-fileicon "swagger" :height 1.0 :v-adjust 0.0 :face all-the-icons-green))
  (add-to-list 'all-the-icons-regexp-icon-alist '("^openapi.json$" all-the-icons-fileicon "swagger" :height 1.0 :v-adjust 0.0 :face all-the-icons-green))
  )

(use-package alert
  :config
  (setq
   alert-default-style 'libnotify
   alert-fade-time 20
   alert-persist-idle-time 60
   )
  )

;; Auto Insert

(use-package autoinsert
  :config
  (setq
   auto-insert-query nil
   auto-insert-directory "~/.emacs.d/insert/"
   auto-insert t
   auto-insert-alist nil
   )
  (add-hook 'find-file-hook 'auto-insert)
  )

;; Undo
(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode)
  )

;; Evil
(use-package evil
  :init
  (setq
   evil-want-keybinding nil
   evil-undo-system 'undo-tree
   )

  :config
  (unbind-key "C-i" evil-motion-state-map)

  (evil-mode 1)
  (evil-add-hjkl-bindings archive-mode-map 'emacs)
  (evil-add-hjkl-bindings messages-buffer-mode-map 'emacs)

  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'display-time-world-mode)
  (evil-set-initial-state 'org-agenda-mode 'motion)

  (general-define-key
   :states 'insert
   "C-v" #'yank
   )
  )

(use-package evil-collection
  :defines evil-collection-mode-list evil-collection-want-unimpaired-p
  :after evil
  :custom
  (evil-collection-want-unimpaired-p nil "Disable binding of square brackets")
  :config
  (setq evil-collection-mode-list
        '(
          deadgrep
          dired
          image
          magit
          xref
          xwidget
          )
        )
  (evil-collection-init)

  (with-eval-after-load 'pdf-tools
    (evil-collection-pdf-setup)
    (general-define-key
     :keymaps 'pdf-view-mode-map
     :states 'normal
     "SPC" nil
     )
    )
  )

(use-package evil-commentary
  :after evil
  :blackout
  :config
  (evil-commentary-mode)
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-org
  :blackout
  :after evil org
  :general
  (:states 'insert
   :keymaps 'org-mode-map
   "<return>" #'evil-org-return
   )
  (:states 'normal
   :keymaps 'org-mode-map
   ;; Motion
   "^" #'evil-org-beginning-of-line
   "$" #'evil-org-end-of-line

   ;; Inserting
   "A" #'evil-org-append-line
   "I" #'evil-org-insert-line
   "o" #'evil-org-open-below
   "O" #'evil-org-open-above

   ;; Move
   "C-K" #'org-metaup
   "C-J" #'org-metadown

   ;; Delete
   "d" #'evil-org-delete
   "x" #'evil-org-delete-char

   ;; Follow
   "<return>" #'org-open-at-point
   )
  (:states '(normal visual)
   :keymaps 'org-mode-map
   "<" #'evil-org-<
   ">" #'evil-org->
   )

  ;; Text Objects
  (:states '(visual operator)
   :keymaps 'org-mode-map
   "ae" #'evil-org-an-object
   "ie" #'evil-org-inner-object
   )

  (:states 'motion
   :keymaps 'org-agenda-mode-map
   "<tab>" #'org-agenda-goto
   "<return>" #'org-agenda-switch-to
   "," #'org-agenda-schedule
   "j" #'org-agenda-next-line
   "k" #'org-agenda-previous-line
   "H" #'org-agenda-do-date-earlier
   "L" #'org-agenda-do-date-later
   "t" #'org-agenda-todo
   "u" #'org-agenda-undo
   "[" #'org-agenda-earlier
   "]" #'org-agenda-later
   ">" #'org-agenda-goto-date
   )
  :config
  ;; Some keys that are more complex to map
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "<C-return>" (evil-org-define-eol-command org-insert-heading-respect-content)
   "<C-S-return>" (evil-org-define-eol-command org-insert-todo-heading-respect-content)
   )
  )

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  )

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install)
  )

;; Git Configuration
(use-package magit
  :general
  (:states 'normal
   :prefix my-leader-key
   "gb" #'magit-branch
   "gB" #'magit-blame
   "gc" #'magit-commit
   "gd" #'magit-diff
   "gp" #'magit-push
   "gr" #'magit-rebase
   "gs" #'magit-status

   "gll" #'magit-log
   "glf" #'magit-log-buffer-file
   )
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends)) ; Disable VC for Git
  ;; magit-extras expects this variable to exist, but for whatever reason, it doesn't for me.
  (defvar project-switch-commands nil)
  )

(use-package git-gutter
  :blackout
  :config
  ;; Not using :general to avoid deferring
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "ghn" #'git-gutter:next-hunk
   "ghp" #'git-gutter:previous-hunk
   )
  (global-git-gutter-mode t)
  )

(use-package browse-at-remote
  :general
  (:states '(normal visual)
   :prefix my-leader-key
   "gp" #'browse-at-remote
   )
  :config
  (setq browse-at-remote-prefer-symbolic nil)
  (add-to-list 'browse-at-remote-remote-type-regexps (cons "^gitlab\\." "gitlab"))
  )

(use-package transient)

(use-package forge
  :after magit
  )

(use-package git-modes)

(add-to-list 'auto-mode-alist '("CODEOWNERS\\'" . conf-mode))

;; Mode line
(use-package doom-modeline
  :after all-the-icons
  :defines doom-modeline-buffer-encoding doom-modeline-buffer-file-name-style doom-modeline-checker-simple-format doom-modeline-minor-modes doom-modeline-mu4e doom-modeline-icon
  :config
  (add-hook 'server-after-make-frame-hook #'doom-modeline-refresh-font-width-cache)
  (setq
   doom-modeline-buffer-encoding nil
   doom-modeline-checker-simple-format nil
   doom-modeline-minor-modes nil
   doom-modeline-buffer-file-name-style 'truncate-with-project
   doom-modeline-mu4e t
   doom-modeline-icon t
   )
  (doom-modeline-mode 1)
  )

;; Theme
(use-package modus-themes
  :straight (:branch "main")
  :config
  (setq modus-themes-headings
        '((t . (rainbow))))
  (setq
   modus-themes-bold-constructs t
   modus-themes-org-agenda '(( scheduled . rainbow))
   )
  (setq shr-color-visible-luminance-min 80)
)

;; Commands
(use-package run-command
  :defines run-command-experiments
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "!" 'run-command
   )
  (setq run-command-experiments '(vterm-run-method))
  )

;; Projectile
(use-package projectile
  :config
  ;; Not using general to avoid deferring
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "pf" #'projectile-find-file
   "pi" #'projectile-invalidate-cache
   "pl" #'projectile-switch-project
   "pK" #'projectile-kill-buffers
   )
  (setq
   projectile-use-git-grep t
   projectile-completion-system 'ivy
   )

  (projectile-mode 1)
  )

;; Ivy
(use-package counsel
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "bb" #'ivy-switch-buffer
   "fR" #'counsel-recentf
   "hf" #'counsel-describe-function
   "hv" #'counsel-describe-variable
   )
  (general-define-key
   "M-x" #'counsel-M-x
   "C-x C-f" #'counsel-find-file
   "C-h f" #'counsel-describe-function
   "C-h v" #'counsel-describe-variable
   )
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "S-SPC" nil ; maps by default to `ivy-restrict-to-matches'
   )
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq
   ivy-count-format "(%d/%d) "
   ivy-use-selectable-prompt t)
  (ivy-mode 1)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  )

(use-package ivy-rich
  :after counsel
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package all-the-icons-ivy-rich
  :after ivy-rich counsel-projectile
  :config
  ;; Only display project names
  (ivy-rich-set-columns
   'counsel-projectile-find-file
   '((all-the-icons-ivy-rich-file-icon)
     (all-the-icons-ivy-rich-project-name)
     ))

  (all-the-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  )

(use-package counsel-projectile
  :after counsel projectile
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "pb" #'counsel-projectile-switch-to-buffer
   "pl" #'counsel-projectile-switch-project
   "pf" #'counsel-projectile-find-file
   "sp" #'counsel-projectile-rg
   )
  (setq
   counsel-projectile-remove-current-project t
   )
  ;; When switching project, run `counsel-project-find-file' in the new project
  (setq
   counsel-projectile-switch-project-action
   (lambda (name)
     (let ((default-directory name))
       (counsel-projectile-find-file)
       )
     )
   )
  )

(use-package prescient)

(use-package ivy-prescient
  :after counsel prescient
  :config
  (ivy-prescient-mode)
  )

(use-package treemacs
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "T" 'treemacs
   )
  (setq
   treemacs-wrap-around nil
   )
  (defun my/is-node_modules (name _)
    (string-equal "node_modules" name))
  (add-to-list 'treemacs-ignored-file-predicates 'my/is-node_modules)
  (treemacs-follow-mode)
  (treemacs-git-mode 'extended)
  )

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-modify-theme "all-the-icons"
    :config
    (progn
      (treemacs-create-icon :icon (format "  %s%s"
                                          (all-the-icons-fileicon "gitlab" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange)
                                          treemacs-all-the-icons-tab)
                            :extensions (".gitlab-ci.yml")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "  %s%s"
                                          (all-the-icons-fileicon "swagger" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-green)
                                          treemacs-all-the-icons-tab)
                            :extensions ("openapi.yaml")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "  %s%s"
                                          (all-the-icons-alltheicon "terminal" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-purple)
                                          treemacs-all-the-icons-tab)
                            :extensions ("gradlew")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "  %s%s"
                                          (all-the-icons-alltheicon "terminal" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-purple)
                                          treemacs-all-the-icons-tab)
                            :extensions ("gradlew.bat")
                            :fallback 'same-as-icon)
      )
    )
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-magit
  :after treemacs magit
  )

;; Web Mode
(use-package web-mode
  :mode ("\\.html\\'" "\\.mustache\\'" "\\.hbs")
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.hbs$" . "html"))
  )

;; JSON
(use-package json-mode
  :mode ("\\.json" "\\.babelrc\\'" "\\.eslintrc\\'")
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'json-mode-map
   "mb" #'json-pretty-print-buffer
   )
  )

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :defines markdown-command
  :config
  (setq
   markdown-command "multimarkdown"
   markdown-fontify-code-blocks-natively t
   )
  )

(use-package edit-indirect
  :after markdown-mode
  )

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  )

;; LaTeX
(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable)
  )

;; Neon
(use-package neon-mode
  :mode "\\.neon\\'"
  )

;; Docker
(use-package dockerfile-mode
  :mode "\\`Dockerfile\\'"
  )

(use-package docker-tramp
  :custom
  (docker-tramp-use-names t "Use container names for accessing containers")
  )

(add-to-list 'auto-mode-alist '("\/\\.env" . conf-mode))

;; Which Key
(use-package which-key
  :blackout
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :blackout
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-dabbrev-downcase nil
   )
  (add-hook 'prog-mode-hook 'company-mode)
  )

;; Rainbow Mode
(use-package rainbow-mode
  :blackout
  :hook css-mode web-mode typescript-mode

  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  )

;; LSP
(use-package lsp-mode
  :hook (
         (conf-javaprop-mode . lsp)
         (java-mode . lsp)
         (js-mode . lsp)
         (kotlin-mode . lsp)
         (rustic-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         )
  :general
  (:states 'normal
   :keymaps 'lsp-mode-map
   :prefix my-leader-key
   "mE" #'lsp-treemacs-errors-list
   "mi" #'lsp-ui-doc-glance
   "mR" #'lsp-rename
   "mx" #'lsp-execute-code-action
   )
  :config

  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-lens-enable t
   lsp-keep-workspace-alive nil
   )

  ;; Enabling mode-specific Flycheck checkers with LSP

  (defvar-local my/flycheck-local-cache nil)

  (defun my/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker my/flycheck-local-cache))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (cond ((derived-mode-p 'typescript-mode)
  ;;                    (setq my/flycheck-local-cache '((lsp . ((next-checkers . (javascript-eslint)))))))
  ;;                   )))

  )

(use-package lsp-ivy
  :after lsp-mode
  :general
  (:states 'normal
   :keymaps 'lsp-mode-map
   :prefix my-leader-key
   "m/" #'lsp-ivy-workspace-symbol
   )
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-enable nil)
  )

(use-package lsp-treemacs
  :after treemacs
  :general
  (:states 'normal
   :keymaps 'lsp-mode-map
   :prefix my-leader-key
   "mr" `(
          ,(lambda () (interactive) (lsp-treemacs-references 1))
          :wk "lsp-treemacs-references"
          )
   )
  )

;; Tree Sitter
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )
(use-package tree-sitter-langs
  :after tree-sitter)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;; Flycheck

(use-package flycheck
  :blackout
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "el" 'flycheck-list-errors
   "en" 'flycheck-next-error
   "ep" 'flycheck-previous-error
   "ev" 'flycheck-verify-setup
   )
  (setq flycheck-global-modes '(not org-mode text-mode))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

;; Shell
(defun my/open-projectile-vterm ()
  "Set up another window, then run `projectile-run-vterm'."
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (projectile-run-vterm)
  )

(defun my/open-vterm-ssh (host user)
  "Open vterm and SSH to HOST as USER."
  (vterm-other-window)
  (vterm-send-string (concat "ssh -l " user " " host "\n"))
  )

(defun my/open-vterm ()
  "Open vterm, SSHing if current directory is remote."
  (interactive)
  (let ((host (file-remote-p default-directory 'host))
        (user (file-remote-p default-directory 'user)))
    (if host
        (my/open-vterm-ssh host user)
      (vterm-other-window)))
  )

(use-package vterm
  :custom
  (vterm-min-window-width 1000 "Make the vterm not wrap lines")
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "'" #'my/open-vterm
   "p'" #'my/open-projectile-vterm
   )
  (general-define-key
   :keymaps 'vterm-mode-map
   "C-w C-w" #'evil-window-next
   "C-w o" #'delete-other-windows
   "C-w C-o" #'delete-other-windows
   )
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)

  (add-hook 'vterm-mode-hook (lambda () (toggle-truncate-lines -1)))
  )

;; Dired

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq
 dired-omit-files (concat dired-omit-files "\\|^\\...+$")
 dired-listing-switches "-alh"
 dired-create-destination-dirs 'ask
 dired-kill-when-opening-new-dired-buffer t
 dired-dwim-target t
 )

(add-hook 'dired-mode-hook 'dired-omit-mode)

(defun dired-open-current-directory ()
  "Run `dired' in the directory of this file."
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'dired-mode-map
 "br" 'revert-buffer
 "mo" 'dired-omit-mode
 )

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "o" 'dired-find-file-other-window
 "s" 'dired-sort-toggle-or-edit
 )

(use-package diredfl
  :config
  (setq diredfl-ignore-compressed-flag nil)
  (add-hook 'dired-mode-hook 'diredfl-mode)
  )

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;; Images
(use-package eimp
  :config
  (add-hook 'image-mode-hook 'eimp-mode)
  )

;; ELisp
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "mn" 'narrow-to-defun
 "mx" '(:ignore t :wk "Eval")
 "mxb" 'eval-buffer
 "mxd" 'eval-defun
 "mxe" 'eval-expression
 )
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "mxr" 'eval-region
 )

(use-package dash
  :config
  (global-dash-fontify-mode)
  )

;; Plain Text
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'text-mode-map
 "mTj" 'table-justify
 "mTs" 'table-span-cell
 "mTS" 'table-split-cell
 )

(use-package string-inflection
  :general
  (:states 'normal
   :prefix my-leader-key
   "t" '(:ignore t :wk "Text")
   "t_" 'string-inflection-underscore
   "tC" 'string-inflection-camelcase
   )
  )

;; Spell Checking
(setq ispell-dictionary "english")
(use-package flyspell
  :blackout
  :hook (org-mode . flyspell-mode)
  )

;; LESS

(use-package less-css-mode
  :mode "\\.less\\'"
  )

;; nginx
(use-package nginx-mode
  :mode "\\`nginx.conf\\'"
  )

;; Org mode
(defun my/enable-org-mode-wordwrap ()
  "Enables word-wrapping for org mode."
  (visual-line-mode))

(defun my/org-expand-all ()
  "Expand an Org document for the things I like."
  (interactive)
  (org-show-all '(blocks headings))
  )

(defun my/cycle-list-or-to-list ()
  "If a headline, `org-toggle-item'.  Else `org-cycle-list-bullet'."
  (interactive)
  (if (org-at-heading-p)
      (org-toggle-item 0)
    (org-cycle-list-bullet))
  )

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  (:states 'normal
   :prefix my-leader-key
   "a" #'org-agenda-list
   "A" #'org-agenda
   "c" #'org-capture
   )
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'org-mode-map
   "m." #'org-time-stamp
   "m," #'org-schedule
   "m/" #'org-sparse-tree
   "ma" #'org-archive-subtree
   "mc" #'org-cycle
   "mC" #'org-global-cycle
   "md" #'org-deadline
   "me" #'org-export-dispatch
   "mE" #'my/org-expand-all
   "mn" #'org-narrow-to-subtree
   "mp" #'org-priority
   "mr" #'org-reveal
   "ms" #'counsel-org-goto
   "mt" #'org-todo

   "mT" '(:ignore t :wk "Tables")
   "mT*" #'org-table-recalculate
   "mTt" #'org-table-iterate
   "mTI" #'org-table-iterate-buffer-tables

   "mTd" '(:ignore t :wk "Delete")
   "mTdr" #'org-table-kill-row
   )
  (:states '(normal visual)
   :prefix my-leader-key
   :keymaps 'org-mode-map
   "m*" #'org-toggle-heading
   "m-" #'my/cycle-list-or-to-list
   "mx" #'org-toggle-checkbox
   )
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  (put 'org-ascii-text-width 'safe-local-variable #'numberp)
  (setq org-ascii-text-width 120)

  ;; Font Configuration
  (my/set-face-attribute 'org-checkbox-statistics-todo nil :family "monospace")
  (my/set-face-attribute 'org-checkbox-statistics-done nil :family "monospace")
  (my/set-face-attribute 'org-link nil :family "monospace")
  (my/set-face-attribute 'org-column-title nil :family "monospace")
  (my/set-face-attribute 'org-meta-line nil :family "monospace")
  (my/set-face-attribute 'org-code nil :family "monospace")
  (my/set-face-attribute 'org-block nil :family "monospace")
  (my/set-face-attribute 'org-meta-line nil :family "monospace")
  (my/set-face-attribute 'org-document-info-keyword nil :family "monospace")
  (my/set-face-attribute 'org-table nil :family "monospace")

  ;; Checkbox Configuration
  ;; Taken from https://jft.home.blog/2019/07/17/use-unicode-symbol-to-display-org-mode-checkboxes/
  (defface org-checkbox-done-text
    '((t (:inherit 'font-lock-comment-face :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  ;; "Local Variables" Configuration
  (defface org-local-variables-headlines
    '((t (:inherit shadow :slant italic :weight ultra-light :height 100)))
    "Face for the Local Variables headline in org-mode.")
  (add-hook
   'org-mode-hook
   (lambda ()
     (font-lock-add-keywords
      nil
      '(("^* Local Variables$" 0 'org-local-variables-headlines t)))
      t
      )
     (font-lock-ensure)
     )

  (defun my/set-org-prettify-symbols ()
    "Set the `prettify-symbols-mode' symbols for org checkboxes."
    (add-to-list 'prettify-symbols-alist '("[ ]" . "☐"))
    (add-to-list 'prettify-symbols-alist '("[X]" . "☑"))
    )
  (add-hook 'org-mode-hook #'my/set-org-prettify-symbols)
  (add-hook 'org-mode-hook #'prettify-symbols-mode 90)

  (setq
   org-directory "~/org"
   org-startup-indented t
   org-startup-folded t
   org-special-ctrl-a/e t
   org-hide-emphasis-markers nil
   org-return-follows-link t
   org-highlight-latex-and-related '(latex script)
   org-list-indent-offset 1
   )
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
  (setq org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "-")))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (add-hook 'org-mode-hook 'my/enable-org-mode-wordwrap)

  ;; Refile
  (setq
   org-refile-targets '((nil . (:maxlevel . 10)))
   org-refile-use-outline-path t
   org-outline-path-complete-in-steps nil
   )

  ;; Export
  (require 'ox-md)
  (setq
   org-html-validation-link nil
   org-html-postamble nil
   org-export-with-toc nil
   )

  (put
   'org-export-initial-scope
   'safe-local-variable
   (lambda (v)
     (memq v '(buffer subtree)))
   )

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  (setq org-confirm-babel-evaluate
        (lambda (language _)
          (memq language '(plantuml)))
        )

  ;; Agenda
  (setq
   org-deadline-warning-days 3
   org-agenda-start-on-weekday nil
   org-agenda-window-setup 'other-window
   org-agenda-span 'day
   )

  (setq org-agenda-time-grid '((daily today require-timed)
                               (1000 1200 1400 1600)
                               "......"
                               "----------------"))
  (setq org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Clocking
  (setq org-clock-idle-time 10)
  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :defines org-superstar-item-bullet-alist
  :config
  (my/set-face-attribute 'org-superstar-item nil :family "monospace")
  (my/set-face-attribute 'org-superstar-header-bullet nil :family "monospace")
  (setq org-superstar-item-bullet-alist '(
                                          (?- . ?➤)
                                          (?+ . "◦")
                                          ))
  )

(use-package gnuplot)

(use-package htmlize)

;; Calendar
(require 'calendar)
(calendar-set-date-style 'iso)
(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])
(setq holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-oriental-holidays nil
      holiday-hebrew-holidays nil)

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern
(setq holiday-christian-holidays
      '((holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")))

(defun my/plantuml-preview-new-window ()
  "Preview the PlantUML in a new window."
  (interactive)
  (plantuml-preview 4))

;; Plant UML
(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'plantuml-mode-map
   "mp" #'my/plantuml-preview-new-window
   )
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "png")
  )

;; Graphviz
(use-package graphviz-dot-mode
  :mode "\\.dot\\'"
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'graphviz-mode-map
   "mp" #'graphviz-dot-preview
   )
  :config
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))

  )

;; Python

(use-package anaconda-mode
  :hook python-mode
  :config
  (add-hook 'python-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :after anaconda-mode company
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

;; Java

(use-package lsp-java
  :after lsp-mode
  :config
  (setq
   lsp-java-implementations-code-lens-enabled t
   )
  )

(use-package lsp-java-boot
  :straight lsp-java
  :after lsp-java
  :config
  (defun my/enable-lsp-java-boot ()
    "Enable `lsp-java-boot' for this buffer."
    (setq-local lsp-java-boot-enabled t)
    )
  (setq lsp-java-boot-enabled nil)
  (add-hook 'java-mode-hook #'my/enable-lsp-java-boot)
  (add-hook 'conf-javaprop-mode-hook #'my/enable-lsp-java-boot)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'conf-javaprop-mode-hook #'lsp-java-boot-lens-mode)
)

(use-package lsp-java-lombok
  :straight (:host github :repo "bricka/lsp-java-lombok" :branch "vmargs-list-and-expand")
  :after lsp-java
  :config
  (setq lsp-java-lombok/enabled t)
  (lsp-java-lombok/init)
  )

(defun my/java-indent-setup ()
  "Define preferred indentation style."
  (progn
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    ))
(add-hook 'java-mode-hook 'my/java-indent-setup)

; Gradle files are written in Groovy
(use-package groovy-mode
  :mode "\\.gradle")

;; Scala

(use-package scala-mode
  :mode "\\.scala\\'")

(use-package lsp-metals
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'scala-mode-map
   "mst" #'lsp-metals-toggle-show-inferred-type
   )
  :config
  (setq
   lsp-metals-show-implicit-conversions-and-classes t
   )
  )

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'rustic-mode-map
   "mt" #'rustic-cargo-test)
  :config
  (setq
   lsp-rust-analyzer-cargo-watch-command "clippy"
   )
  )

;; TypeScript

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  )

(use-package add-node-modules-path
  :hook (typescript-mode . add-node-modules-path)
  )

(use-package prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode))
  )

(defun run-command-recipe-package-json--get-scripts (package-json-file)
  "Extract NPM scripts from PACKAGE-JSON-FILE."
  (with-temp-buffer
    (insert-file-contents package-json-file)
    (when-let ((script-hash (gethash "scripts" (json-parse-buffer))))
      (let (scripts '())
        (maphash (lambda (key _value) (push key scripts)) script-hash)
        scripts))))

(defun run-command-recipe-package-json ()
  "Recipes for scripts in package.json."
  (when-let* ((project-dir
               (locate-dominating-file default-directory "package.json"))
              (scripts
               (run-command-recipe-package-json--get-scripts (concat project-dir "package.json")))
              (script-runner
               (if (file-exists-p (concat project-dir "yarn.lock")) "yarn" "npm")))
    (mapcar (lambda (script)
              (list :command-name script
                    :command-line (concat script-runner " run " script)
                    :display script
                    :working-dir project-dir))
            scripts)))


(add-to-list 'run-command-recipes #'run-command-recipe-package-json)

(defun run-command-recipe-gradle ()
  "Recipes for scripts that use Gradle."
  (when-let* ((project-dir (locate-dominating-file default-directory "gradlew"))
              (maybe-gradlew (concat project-dir "gradlew"))
              (gradlew (when (file-exists-p maybe-gradlew) maybe-gradlew)))
    (mapcar (lambda (task)
              (list :command-name task
                    :command-line (concat gradlew " " task)))
            '("build"))
    ))

(add-to-list 'run-command-recipes #'run-command-recipe-gradle)

;; CSV
(use-package csv-mode)

;; XML

(use-package nxml-mode
  :straight (:type built-in)
  :mode "\\.xml"
  :config
  (setq nxml-slash-auto-complete-flag t)
  )

;; Kotlin
(use-package kotlin-mode
  :mode "\\.kt\\'" "\\.kts\\'"
  )

;; PDF
(use-package pdf-tools
  :config
  (pdf-tools-install)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  )

;; PEM
(use-package x509-mode
  :commands x509-dwim
  :config
  (general-define-key
   :states 'normal
   :keymaps x509-mode-map
   "q" #'quit-window
   )
  )
(require 'pem-mode)

;; File Keys
(defun my/visit-emacs-init ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; These are taken from Steve Yegge's .emacs:
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun my/rename-file-and-buffer (new-name)
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

(defun my/move-buffer-file (dir)
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

(defun my/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (cond ((vc-backend filename) (vc-delete-file filename))
            (t (progn
                 (delete-file filename)
                 (message "Deleted file %s" filename)
                 (kill-buffer)))))))

(general-define-key
 :prefix my-leader-key
 :states 'normal
 "fe" #'my/visit-emacs-init
 "fm" #'my/move-buffer-file
 "fr" #'my/rename-file-and-buffer
 )

;; Window Keys
(general-define-key
 :prefix my-leader-key
 :states 'normal
 "w-" #'split-window-below
 "w/" #'split-window-right
 "w=" #'balance-windows
 "wn" #'make-frame
 )

(defun evil-ex-define-cmd-local
    (cmd function)
  "Locally binds command CMD to the function FUNCTION."
  (unless (local-variable-p 'evil-ex-commands)
    (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
  (evil-ex-define-cmd cmd function))

(defun my/enable-evil-mode-for-edit-with-emacs ()
  "Make Evil mode work with edit-server-edit-mode."
  (progn
    (evil-ex-define-cmd-local "wq" 'edit-server-save)
    (evil-ex-define-cmd-local "w[rite]" 'edit-server-save)))

(use-package edit-server
  :config
  (edit-server-start)
  (add-hook 'edit-server-edit-mode-hook 'my/enable-evil-mode-for-edit-with-emacs)

  (add-to-list 'edit-server-url-major-mode-alist '("wikifiniens" . mediawiki-mode))
  )

;; Time
(use-package time
  :config
  (setq
   zoneinfo-style-world-list '(("Europe/Berlin" "München"))
   display-time-format "%Y-%m-%d %H:%M"
   display-time-default-load-average nil
   )
  )

;; Pomodoro

;; (use-package pomidor
;;   :config
;;   (defun my/pomidor-mode-line ()
;;     "Calculate what string should appear in the modeline."
;;     (if (not (pomidor-running-p))
;;         nil
;;       (cond
;;        ((pomidor-overwork-p) (propertize "🍅 TAKE BREAK" 'face 'success))
;;        ((pomidor-break-over-p) (propertize "🍅 BREAK OVER" 'face 'warning))
;;        ((pomidor-break-duration)
;;         (propertize
;;          (format-time-string
;;           "🍅 %M:%S" (time-subtract
;;                       (seconds-to-time pomidor-break-seconds)
;;                       (pomidor-break-duration)))
;;          'face 'success))
;;        (t (format-time-string "🍅 %M:%S" (pomidor-total-duration)))
;;        )
;;       ))
;;   (setq
;;    pomidor-sound-tick nil
;;    pomidor-sound-tack nil
;;    )
;;   (add-to-list
;;    'global-mode-string
;;    '(:eval (my/pomidor-mode-line))
;;    t)
;;   (add-to-list 'evil-emacs-state-modes 'pomidor-mode)
;;   )

(use-package journalctl
  :straight (:host gitlab :repo "bricka/emacs-journalctl")
  :general
  (:prefix my-leader-key
   :states 'normal
   "jj" #'journalctl
   "ju" #'journalctl-user
   )
  :config
  (general-define-key
   :keymaps 'journalctl-mode-map
   :states 'normal
   "gr" #'journalctl-refresh
   "q" #'quit-window
   "ZZ" #'quit-window)
  )

(use-package deadgrep
  :config
  (setq deadgrep-project-root-function #'projectile-project-root)
  )

(use-package ledger-mode
  :mode "\\.ledger\\'" "\\.timedot"
  :config
  ;; (setq ledger-binary-path "hledger")
  (add-hook 'ledger-mode-hook #'company-mode)
  (general-define-key
   :keymaps 'ledger-mode-map
   :states 'normal
   "[" #'ledger-navigate-prev-xact-or-directive
   "]" #'ledger-navigate-next-xact-or-directive
   )
  (general-define-key
   :prefix my-leader-key
   :keymaps 'ledger-mode-map
   :states 'normal
   "mr" #'ledger-report
   )
  )

;; Cannot enabled flycheck-hledger, because ledger-mode seems to not work with "hledger" for me
;; (use-package flycheck-hledger
;;   :after (flycheck ledger-mode)
;;   )

;; Local Configuration
(defvar local-config-location
  (let ((val (getenv "EMACS_LOCAL_CONFIG_PATH")))
    (and
     val
     (expand-file-name val)
     ))
  "Location of a local configuration file.

Value is taken from EMACS_LOCAL_CONFIG_PATH environment variable."
  )

(when (and local-config-location (file-exists-p local-config-location))
    (defun my/visit-local-config ()
      "Visit the local configuration file."
      (interactive)
      (find-file local-config-location)
      )
    (general-define-key
     :states 'normal
     :prefix my-leader-key
     "fl" #'my/visit-local-config
     )
    (let ((straight-current-profile 'local))
      (load-file local-config-location)
      ))

(server-start)

(provide 'init)
;;; init.el ends here
