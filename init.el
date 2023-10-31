;;; init.el --- Alex Brick's Emacs configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

(when (treesit-available-p)
  (require 'treesit))

;; URL
(setq url-privacy-level 'paranoid)
(url-setup-privacy-info)

(setq load-path
      (append load-path
              (directory-files (concat (expand-file-name user-emacs-directory) "packages") t directory-files-no-dot-files-regexp t)))

;; Elpaca Setup

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Disable Menus
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Straightforward config
(setq-default
 indent-tabs-mode nil
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
 split-width-threshold 200
 sentence-end-double-space nil
 require-final-newline t
 make-backup-files nil
 )

(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

(recentf-mode)

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
    (mapcar
     (lambda (l) (string-trim-right (concat ";; " l)))
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
  "Ideally used as a hook to disable `auto-fill-mode'."
  (auto-fill-mode -1)
  )

(defun my/disable-auto-save-mode ()
  "Ideally used as a hook to disable `auto-save-mode'."
  (auto-save-mode -1)
  )

(use-package ht)

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

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

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
   "bb" #'switch-to-buffer
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

(elpaca-wait)

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
   "hf" #'helpful-callable
   "hk" #'helpful-key
   "hv" #'helpful-variable
   )
  ("C-h f" #'helpful-function
   "C-h k" #'helpful-key
   "C-h v" #'helpful-variable
   "C-h x" #'helpful-command
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

(elpaca-wait)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(setq prettify-symbols-unprettify-at-point 'right-edge)

;; PATH from Shell

(when (memq system-type '(darwin gnu/linux))
  (use-package exec-path-from-shell
    :config
    (setq exec-path-from-shell-arguments nil)
    (add-to-list 'exec-path-from-shell-variables "EMACS_LOCAL_CONFIG_PATH")
    (add-to-list 'exec-path-from-shell-variables "JAVA_HOME")
    (exec-path-from-shell-initialize)
    )
  )

(use-package nerd-icons
  :config
  (dolist (ext '("npmignore" "npmrc"))
    (add-to-list 'nerd-icons-extension-icon-alist `(,ext nerd-icons-sucicon "nf-seti-npm" :face nerd-icons-dred))
    (add-to-list 'nerd-icons-regexp-icon-alist `(,(concat "^\\." ext "$") nerd-icons-sucicon "nf-seti-npm" :face nerd-icons-dred))
    )

  (add-to-list 'nerd-icons-extension-icon-alist '("properties" nerd-icons-codicon "nf-cod-settings" :face nerd-icons-dyellow))
  (add-to-list 'nerd-icons-extension-icon-alist '("json" nerd-icons-mdicon "nf-md-code_json" :face nerd-icons-yellow))
  (add-to-list 'nerd-icons-extension-icon-alist '("proto" nerd-icons-octicon "nf-oct-container" :face nerd-icons-dblue))

  (add-to-list 'nerd-icons-regexp-icon-alist '("\\.gradle.kts$" nerd-icons-sucicon "nf-seti-gradle" :face nerd-icons-silver))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^tsconfig.*\\.json$" nerd-icons-sucicon "nf-seti-tsconfig" :face nerd-icons-blue-alt))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^gradlew$" nerd-icons-devicon "nf-dev-terminal" :face nerd-icons-purple))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^COPYING\\|LICENSE$" nerd-icons-codicon "nf-cod-law" :face nerd-icons-silver))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^\\.gitlab-ci.yml$" nerd-icons-mdicon "nf-md-gitlab" :face nerd-icons-orange))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^Makefile$" nerd-icons-devicon "nf-dev-gnu" :face nerd-icons-dorange))
  (add-to-list 'nerd-icons-regexp-icon-alist '("^\\.clang-format$" nerd-icons-sucicon "nf-seti-stylelint" :face nerd-icons-orange))
  )

(use-package alert
  :config
  (setq alert-default-style
        (if (eq system-type 'darwin)
            'osx-notifier
          'libnotify))
  (setq
   alert-fade-time 20
   alert-persist-idle-time 60
   )
  )

;; Auto Insert

(defun my/package-name-for-file ()
  "Determine the package name for this Kotlin or Java file."
  (let ((without-prefix (replace-regexp-in-string "^.*\\(main\\|test\\)/\\(kotlin\\|java\\)/" "" (file-name-directory (buffer-file-name)))))
    (file-name-base (string-replace "/" "." without-prefix))))

(use-package autoinsert
  :elpaca nil
  :config
  (setq
   auto-insert-query nil
   auto-insert-directory "~/.emacs.d/insert/"
   auto-insert t
   auto-insert-alist nil
   )
  (add-hook 'find-file-hook #'auto-insert)

  (add-to-list
   'auto-insert-alist
   '((kotlin-ts-mode . "Kotlin source file")
     nil
     "package "
     (my/package-name-for-file)
     "\n\n"
     "class " (file-name-base (buffer-file-name))
     ))
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
          compile
          deadgrep
          dired
          docker
          flycheck
          image
          magit
          vterm
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
   "B" #'org-agenda-bulk-action
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

;; Kill Ring
(use-package browse-kill-ring
  :commands browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t))

;; Git Configuration
(use-package magit
  :general
  (:states 'normal
   :prefix my-leader-key
   "gb" #'magit-branch
   "gB" #'magit-blame
   "gc" #'magit-commit
   "gd" '(:ignore t :which-key "Diff")
   "gdd" #'magit-diff
   "gdu" `(
           ,(lambda () (interactive) (magit-diff-range "@{upstream}"))
           :wk "upstream"
           )
   "gp" #'magit-push
   "gr" #'magit-rebase
   "gs" #'magit-status

   "gll" #'magit-log
   "glf" #'magit-log-buffer-file
   )
  :config
  (setq git-commit-summary-max-length 100)
  )

(use-package diff-hl
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "ghn" #'diff-hl-next-hunk
   "ghp" #'diff-hl-previous-hunk)
  (global-diff-hl-mode)
  (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  )

(use-package browse-at-remote
  :general
  (:states '(normal visual)
   :prefix my-leader-key
   "gp" #'browse-at-remote
   )
  :config
  (setq browse-at-remote-prefer-symbolic nil)
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^gitlab\\." :type "gitlab"))
  )

(use-package transient)

(use-package forge
  :after magit
  )

(use-package git-modes)

(add-to-list 'auto-mode-alist '("CODEOWNERS\\'" . conf-mode))

;; Mode line
(use-package doom-modeline
  :defines doom-modeline-buffer-encoding doom-modeline-buffer-file-name-style doom-modeline-checker-simple-format doom-modeline-minor-modes doom-modeline-mu4e doom-modeline-icon
  :config
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
  :config
  (setq modus-themes-headings
        '((t . (rainbow))))
  (setq
   modus-themes-bold-constructs t
   modus-themes-org-agenda '(( scheduled . rainbow))
   )
  (setq shr-color-visible-luminance-min 80)

  (defun my/run-face-hooks ()
    "Run my custom face hooks stored in `face-hooks'."
    (run-hooks 'face-hooks))
  (advice-add 'modus-themes-toggle :after #'my/run-face-hooks)
)

(use-package ef-themes
  :config
  (setq ef-themes-mixed-fonts t)
  )

;; Commands
(use-package run-command
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "!" #'run-command
   )
  (setq run-command-default-runner #'run-command-runner-compile)
  )

;; Project Management
(defun my/project-vterm ()
  "Run `vterm' for this project."
  (interactive)
  (split-window-sensibly)
  (let ((default-directory (project-root (project-current)))
        (vterm-buffer-name (format "*vterm[%s]*" (project-name (project-current)))))
    (vterm)))

(use-package project
  :elpaca nil
  :general
  (:states 'normal
   :prefix my-leader-key
   "p'" #'my/project-vterm
   "pd" #'project-dired
   "pf" #'project-find-file
   "pl" #'project-switch-project
   "pK" #'project-kill-buffers)
  :config
  (setq project-switch-commands #'project-find-file)
  )

;; Minibuffer Completion

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(use-package orderless
  :config
  ;; We need this in order to work with Tramp:
  ;; https://github.com/minad/vertico#tramp-hostname-and-username-completion
  ;; Hopefully it can be removed with Emacs 30.
  (setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :elpaca (:files (:defaults "extensions/vertico-directory.el"))
  :config
  (vertico-mode))

(use-package vertico-directory
  :elpaca nil
  :after vertico
  :config
  (general-define-key
   :keymaps 'vertico-map
   "C--" #'vertico-directory-up
   "RET" #'vertico-directory-enter)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package marginalia
  :config
  (marginalia-mode))

;; Treemacs

(use-package treemacs
  :config
  (defun my/treemacs-prev-workspace ()
    "Select the previous workspace.

Like `treemacs-next-workspace' with a prefix arg."
    (interactive)
    (treemacs-next-workspace 1))

  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "T" '(:ignore t :wk "Treemacs")
   "TT" #'treemacs
   "T]" #'treemacs-next-workspace
   "T[" #'my/treemacs-prev-workspace
   )
  (general-define-key
   :states 'motion
   :keymaps 'treemacs-mode-map
   "]]" #'treemacs-next-workspace
   )
  (setq
   treemacs-wrap-around nil
   )
  (defun my/is-node_modules (name _)
    (string-equal "node_modules" name))
  (add-to-list 'treemacs-ignored-file-predicates #'my/is-node_modules)
  (treemacs-follow-mode)
  (treemacs-git-mode 'extended)
  )

(use-package treemacs-evil
  :after treemacs evil)

(defmacro my/create-treemacs-nerd-icon (func icon face extensions)
  "Expands to the correct form for `treemacs-create-icon' to add a custom icon."
  `(treemacs-create-icon :icon (format "  %s%s" (,func ,icon :face ,face :v-adjust -0.05 :height 1.0) treemacs-nerd-icons-tab)
                         :extensions ,extensions
                         :fallback 'same-as-icon))

(use-package treemacs-nerd-icons
  :config
  (treemacs-modify-theme "nerd-icons"
    :config
    (progn
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-gradle" 'nerd-icons-silver ("build.gradle.kts" "settings.gradle.kts"))
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-npm" 'nerd-icons-red ("package.json"))
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-tsconfig" 'nerd-icons-blue-alt ("tsconfig.json"))
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-tsconfig" 'nerd-icons-blue-alt ("tsconfig.dev.json"))
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-yarn" 'nerd-icons-blue-alt ("yarn.lock"))
      (my/create-treemacs-nerd-icon nerd-icons-mdicon "nf-md-code_json" 'nerd-icons-yellow ("json"))
      (my/create-treemacs-nerd-icon nerd-icons-mdicon "nf-md-gitlab" 'nerd-icons-orange (".gitlab-ci.yml"))
      (my/create-treemacs-nerd-icon nerd-icons-devicon "nf-dev-gnu" 'nerd-icons-dorange ("Makefile"))
      (my/create-treemacs-nerd-icon nerd-icons-devicon "nf-dev-terminal" 'nerd-icons-purple ("gradlew"))
      (my/create-treemacs-nerd-icon nerd-icons-sucicon "nf-seti-stylelint" 'nerd-icons-orange (".clang-format"))

      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_right" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("src-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_down" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("src-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_right" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("build-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_down" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("build-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_right" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("test-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_down" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("test-open")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_right" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("tmp-closed")
                            :fallback 'same-as-icon)
      (treemacs-create-icon :icon (format "%s%s%s%s"
                                          (nerd-icons-octicon "nf-oct-chevron_down" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab
                                          (nerd-icons-octicon "nf-oct-file_directory" :face 'treemacs-nerd-icons-file-face)
                                          treemacs-nerd-icons-tab)
                            :extensions ("tmp-open")
                            :fallback 'same-as-icon)
      ))
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after treemacs magit
  )

;; Man pages

;; On Mac OS X, a lot of man pages are built in a WoMan-incompatible
;; way. So we want to use "man" instead. However, the default Mac OS X
;; "man" program does not use a cache, which makes the command take a
;; long time. So we need to install "man-db" from Homebrew and use its
;; "man" command instead.
;;
;; If a command doesn't appear, try refreshing the cache by running
;; "mandb".
(when (eq system-type 'darwin)
  (setq manual-program "gman"
        Man-notify-method 'aggressive)
  (general-define-key
   :states 'motion
   "K" #'man
   ))

;; Compilation
(defun my/compile-ansi-color ()
  "Apply ANSI color to a line in compile-mode."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(use-package compile
  :elpaca nil
  :config
  (setq compilation-scroll-output 'first-error
        compilation-max-output-line-length nil)
  (add-to-list 'safe-local-variable-values '(compilation-read-command . nil)))

(use-package fancy-compilation
  :config
  (setq fancy-compilation-override-colors nil)
  (fancy-compilation-mode))

;; Web Mode
(use-package web-mode
  :mode ("\\.html\\'" "\\.mustache\\'" "\\.hbs")
  :config
  (add-to-list 'lsp-language-id-configuration '(".*\\.hbs$" . "html"))
  )

;; JSON
(defun my/pretty-print-json-buffer ()
  "Pretty print the JSON in the current buffer."
  (interactive)
  (if (not (executable-find "json_pp"))
      (error "json_pp is not in PATH")
    (call-process-region nil nil "json_pp" t t nil "-json_opt" "space_after,indent,utf8")))

(use-package jsonian
  :elpaca (:host github :repo "iwahbe/jsonian")
  :mode (("\\.json" . jsonian-mode)
         ("\\.babelrc" . jsonian-mode)
         (".eslintrc" . jsonian-mode))
  :config
  (jsonian-enable-flycheck)
  (general-define-key
   :keymaps 'jsonian-mode-map
   :states 'normal
   :prefix my-leader-key
   "mB" #'my/pretty-print-json-buffer
   )
  )

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :defines markdown-command
  :config
  (general-define-key
   :states 'normal
   :keymaps 'markdown-mode-map
   "RET" #'markdown-follow-thing-at-point)
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
(use-package tex
  :straight auctex
  :config
  (general-define-key
   :states 'normal
   :keymaps 'LaTeX-mode-map
   :prefix my-leader-key
   "mp" #'preview-document
   )
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq-default TeX-engine 'xetex)
  (setq TeX-electric-math '("$" . "$"))

  (add-hook 'TeX-mode-hook #'prettify-symbols-mode)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

;; Neon
(use-package neon-mode
  :mode "\\.neon\\'"
  )

;; Docker
(use-package dockerfile-mode
  :mode "\\`Dockerfile\\'"
  )

(use-package docker
  :commands docker)

(add-to-list 'auto-mode-alist '("\/\\.env" . conf-mode))

;; Which Key
(use-package which-key
  :blackout
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :hook ((prog-mode . company-mode))
  :blackout
  :config
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-dabbrev-downcase nil
   )
  )

;; Rainbow Mode
(use-package rainbow-mode
  :blackout
  :hook css-mode web-mode typescript-mode

  :config
  (add-to-list 'rainbow-html-colors-major-mode-list #'less-css-mode)
  )

;; LSP

;; Enabling mode-specific Flycheck checkers with LSP
;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(use-package lsp-mode
  :hook (
         (conf-javaprop-mode . lsp)
         (java-mode . lsp)
         (js-mode . lsp)
         (js-ts-mode . lsp)
         (kotlin-mode . lsp)
         (kotlin-ts-mode . lsp)
         (rustic-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (tsx-ts-mode . lsp)
         (typescript-mode . lsp)
         (typescript-ts-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         )
  :config
  (defun my/lsp-workspace-restart-confirm ()
    "Like `lsp-workspace-restart', but ask for confirmation first."
    (interactive)
    (when (yes-or-no-p "Do you want to restart the LSP workspace? ")
      (call-interactively #'lsp-workspace-restart)))

  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   :prefix my-leader-key
   "mE" #'lsp-treemacs-errors-list
   "mi" #'lsp-ui-doc-glance
   "mI" #'lsp-inlay-hints-mode
   "mR" #'lsp-rename
   "mx" #'lsp-execute-code-action
   "mw" '(:ignore t :wk "Workspace")
   "mwd" #'lsp-describe-session
   "mwr" #'my/lsp-workspace-restart-confirm)

  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-lens-enable t
   lsp-keep-workspace-alive nil
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

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;; Treesit
(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist '(kotlin . ("https://github.com/fwcd/tree-sitter-kotlin")))
  (add-to-list 'treesit-language-source-alist '(mermaid . ("https://github.com/monaqa/tree-sitter-mermaid")))
  (add-to-list 'treesit-language-source-alist '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git")))
  (add-to-list 'treesit-language-source-alist '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src")))
  (add-to-list 'treesit-language-source-alist '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))))

;; Flycheck

(use-package flycheck
  :blackout
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "el" #'flycheck-list-errors
   "en" #'flycheck-next-error
   "ep" #'flycheck-previous-error
   "ev" #'flycheck-verify-setup
   )

  (setq flycheck-global-modes '(not org-mode text-mode))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)

  (flycheck-add-mode 'javascript-eslint #'web-mode)

  ;; Enabling mode-specific Flycheck checkers with LSP
  ;; https://github.com/flycheck/flycheck/issues/1762
  (advice-add #'flycheck-checker-get :around #'my/flycheck-checker-get)
  )

;; Shell
(defun my/leave-vterm-copy-mode ()
  (vterm-copy-mode -1))

(use-package vterm
  :general
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "'" #'vterm-other-window
   )
  (general-define-key
   :keymaps 'vterm-mode-map
   :states 'insert
   "C-<escape>" #'vterm-send-escape
   "C-w C-w" #'evil-window-next
   )
  (add-hook
   'vterm-mode-hook
   (lambda ()
     (add-hook 'evil-normal-state-entry-hook #'vterm-copy-mode nil :local)
     (add-hook 'evil-normal-state-exit-hook #'my/leave-vterm-copy-mode nil :local)))
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

(add-hook 'dired-mode-hook #'dired-omit-mode)

(defun dired-open-current-directory ()
  "Run `dired' in the directory of this file."
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'dired-mode-map
 "br" #'revert-buffer
 "mo" #'dired-omit-mode
 )

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "o" #'dired-find-file-other-window
 "s" #'dired-sort-toggle-or-edit
 )

(use-package diredfl
  :hook ((dired-mode . diredfl-mode))
  :config
  (setq diredfl-ignore-compressed-flag nil)
  )

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Images
(use-package eimp
  :hook ((image-mode . eimp-mode))
  )

(add-hook 'image-mode-hook #'auto-revert-mode)

;; ELisp
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "mn" #'narrow-to-defun
 "mx" '(:ignore t :wk "Eval")
 "mxb" #'eval-buffer
 "mxd" #'eval-defun
 "mxe" #'eval-expression
 )
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "mxr" #'eval-region
 )

(add-hook 'emacs-lisp-mode-hook (lambda () (prettify-symbols-mode 1)))

(use-package dash
  :config
  (global-dash-fontify-mode)
  )

(use-package flycheck-package
  :after flycheck
  :config
  (flycheck-package-setup)
  )

;; Plain Text
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'text-mode-map
 "mTj" #'table-justify
 "mTs" #'table-span-cell
 "mTS" #'table-split-cell
 )

(use-package string-inflection
  :general
  (:states 'normal
   :prefix my-leader-key
   "t" '(:ignore t :wk "Text")
   "t_" #'string-inflection-underscore
   "tC" #'string-inflection-camelcase
   )
  )

;; Spell Checking
(defun my/jinx-correct-buffer ()
  "Run `jinx-correct' on the full buffer."
  (interactive)
  (jinx-correct :all))

(use-package jinx
  :hook ((org-mode . jinx-mode)
         (TeX-mode . jinx-mode))
  :general
  ("M-$" #'jinx-correct)
  (:states 'normal
   "z=" #'jinx-correct
   "z+" #'my/jinx-correct-buffer)
  :config
  (setq jinx-languages "en_US de_DE")
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

;; Latest versions of org are broken with regards to links:
;; https://github.com/org-roam/org-roam/issues/2361
;; Need to manually check out an older version:
;; git co release_9.6.7
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :general
  (:states 'normal
   :prefix my-leader-key
   "a" #'org-agenda-list
   "A" #'org-agenda
   "c" #'org-capture
   )
  :config
  ;; Keybindings in Org files
  (general-define-key
   :states 'normal
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
   "mI" #'org-redisplay-inline-images
   "mn" #'org-narrow-to-subtree
   "mp" #'org-priority
   "mr" #'org-reveal
   "ms" #'org-goto
   "mt" #'org-todo

   "mT" '(:ignore t :wk "Tables")
   "mT*" #'org-table-recalculate
   "mTt" #'org-table-iterate
   "mTI" #'org-table-iterate-buffer-tables

   "mTd" '(:ignore t :wk "Delete")
   "mTdr" #'org-table-kill-row
   )
  (general-define-key
   :states '(normal visual)
   :prefix my-leader-key
   :keymaps 'org-mode-map
   "m*" #'org-toggle-heading
   "m-" #'my/cycle-list-or-to-list
   "mx" #'org-toggle-checkbox
   )

  (add-hook 'org-mode-hook #'variable-pitch-mode)

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

  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  (add-hook 'org-mode-hook #'my/enable-org-mode-wordwrap)

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
   org-table-export-default-format "orgtbl-to-csv"
   )

  (put
   'org-export-initial-scope
   'safe-local-variable
   (lambda (v)
     (memq v '(buffer subtree)))
   )

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-src-lang-modes '("mermaid" . mermaid-ts))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))
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
  (setq org-agenda-current-time-string "ᗕ┈┈┈┈┈┈┈ now")
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Clocking
  (setq org-clock-idle-time 10)

  ;; Source code
  (add-to-list 'org-src-lang-modes '("json" . jsonian))
  (add-to-list 'org-src-lang-modes '("kotlin" . kotlin-ts))
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

(use-package org-pdftools
  :after org
  :config
  (org-pdftools-setup-link)
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
        (holiday-fixed 5 1 "Maifeiertag")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern
(setq holiday-christian-holidays
      '((holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")))

;; Plant UML
(defun my/plantuml-preview-new-window ()
  "Preview the PlantUML in a new window."
  (interactive)
  (plantuml-preview 4))

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

;; Mermaid
(use-package mermaid-ts-mode
  :elpaca (:host gitlab :repo "bricka/emacs-mermaid-ts-mode")
  :mode "\\.mmd\\'"
  :config
  ;; I still want to have `rainbow-delimiters-mode' enabled for most
  ;; programming modes, but it doesn't make sense for Mermaid Mode,
  ;; since parens are used as arrow heads
  (add-hook 'mermaid-ts-mode-hook #'(lambda () (rainbow-delimiters-mode -1)))
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
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode)
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
  :elpaca (:host github :repo "bricka/lsp-java-lombok" :branch "vmargs-list-and-expand")
  :after lsp-java
  :config
  (setq lsp-java-lombok/enabled t)
  (lsp-java-lombok/init)
  )

(defun my/java-indent-setup ()
  "Define preferred indentation style."
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  )
(add-hook 'java-mode-hook #'my/java-indent-setup)

; Gradle files are written in Groovy
(use-package groovy-mode
  :mode "\\.gradle\\'")

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

;; TaskJuggler
(use-package taskjuggler-mode
  :elpaca (:host gitlab :repo "bricka/emacs-taskjuggler-mode")
  :mode "\\.tjp"
  )

(use-package flycheck-taskjuggler
  :elpaca (:host gitlab :repo "bricka/emacs-flycheck-taskjuggler")
  :after taskjuggler-mode)

;; TypeScript

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

(use-package add-node-modules-path
  :hook typescript-mode typescript-ts-mode js-mode js-ts-mode
  :config
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\""))
  )

(use-package prettier-js
  :hook ((typescript-mode . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (js-ts-mode . prettier-js-mode))
  )

(defun run-command-recipe-package-json--get-scripts (package-json-file)
  "Extract NPM scripts from PACKAGE-JSON-FILE."
  (with-temp-buffer
    (insert-file-contents package-json-file)
    (when-let ((script-hash (gethash "scripts" (json-parse-buffer))))
      (ht-keys script-hash))))

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

(defvar my/spring-boot-p nil
  "This repo uses Spring Boot.")
(put 'my/spring-boot-p 'safe-local-variable #'booleanp)

(use-package run-command-gradle
  :elpaca (:host gitlab :repo "bricka/emacs-run-command-gradle")
  :config
  (setq run-command-gradle-tasks
        (list
         (make-run-command-gradle-task
          :task "build"
          :compile-p t
          :project-local-p t
          )
         (make-run-command-gradle-task
          :task "check"
          :compile-p t
          :project-local-p t
          )
         (make-run-command-gradle-task
          :task "clean")
         (make-run-command-gradle-task
          :task "test"
          :compile-p t
          :project-local-p t)
         (make-run-command-gradle-task
          :task "run"
          :project-local-p t
          :predicate (lambda () (null my/spring-boot-p)))
         (make-run-command-gradle-task
          :task "bootRun"
          :project-local-p t
          :predicate (lambda () my/spring-boot-p))
         (make-run-command-gradle-task
          :task "bootRun"
          :project-local-p t
          :predicate (lambda () my/spring-boot-p)
          :name-comment "local"
          :arguments "--args='--spring.profiles.active=local'")
         )))

;; Gradle compilation error/warn matchers
(add-to-list 'compilation-error-regexp-alist '("^e: file://\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 2))
(add-to-list 'compilation-error-regexp-alist '("^w: file://\\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 1))

(defun run-command-recipe-docker-compose ()
  "Recipes for Docker Compose files."
  (when-let ((file-path (buffer-file-name))
             (file-name (file-name-nondirectory file-path)))
    (let ((case-fold-search t))
      (when (string-match "docker-compose\\.ya?ml" file-name)
        (list
         (list :command-name "up"
               :command-line "docker-compose up -d --remove-orphans"))))))

(add-to-list 'run-command-recipes #'run-command-recipe-docker-compose)

;; CSV
(use-package csv-mode)

;; XML

(use-package nxml-mode
  :elpaca nil
  :mode "\\.xml"
  :config
  (setq nxml-slash-auto-complete-flag t)
  )

;; Kotlin
(use-package kotlin-ts-mode
  :mode "\\.kt\\'" "\\.kts\\'"
  :config
  (general-define-key
   :states 'normal
   :keymaps 'kotlin-ts-mode-map
   :prefix my-leader-key
   "mg" '(:ignore t :wk "Goto")
   "mgt" #'kotlin-ts-mode-goto-test-file
   "mt" '(:ignore t :wk "Test")
   "mtc" #'kotlin-ts-mode-run-current-test-class
   "mtf" #'kotlin-ts-mode-run-current-test-function
   )
  )

(flycheck-def-config-file-var flycheck-editorconfig ktlint ".editorconfig")

(flycheck-define-checker ktlint
  "A checker using ktlint."
  :command ("ktlint" source (config-file "--editorconfig" flycheck-editorconfig))
  ;; :command ("ktlint" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": " (message) "(" (id (one-or-more (in alpha ":" "-"))) ")" line-end))
  :modes (kotlin-mode kotlin-ts-mode)
  )

(add-to-list 'flycheck-checkers 'ktlint)

(with-eval-after-load 'lsp-mode
    (add-hook 'lsp-managed-mode-hook
              (lambda ()
                (when (derived-mode-p 'kotlin-ts-mode)
                  (setq my/flycheck-local-cache '((lsp . ((next-checkers . (ktlint))))))))))

(use-package ktlint-format-mode
  :elpaca (:host gitlab :repo "bricka/emacs-ktlint-format-mode")
  :hook kotlin-mode kotlin-ts-mode)

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
   :keymaps 'x509-mode-map
   "q" #'quit-window
   )
  )
(require 'pem-mode)

;; File Keys
(defun my/visit-emacs-init ()
  "Visit ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; Inspired from Steve Yegge's .emacs:
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun my/move-buffer-file (dest)
  "Move current file to DEST."
  (interactive "FNew location: ")
  (let* ((current-name (buffer-file-name)))
    (if (not current-name)
        (message "Buffer '%s' is not visiting a file!" current-name)
      (rename-file current-name dest 1)
      (set-visited-file-name dest :no-query :along-with-file))))

(defun my/kill-file-path ()
  "Copy the path of the current file."
  (interactive)
  (kill-new (buffer-file-name)))

(general-define-key
 :prefix my-leader-key
 :states 'normal
 "fe" #'my/visit-emacs-init
 "fm" #'my/move-buffer-file
 "fP" #'my/kill-file-path
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
  (evil-ex-define-cmd-local "wq" #'edit-server-save)
  (evil-ex-define-cmd-local "w[rite]" #'edit-server-save))

(use-package edit-server
  :config
  (edit-server-start)
  (add-hook 'edit-server-edit-mode-hook #'my/enable-evil-mode-for-edit-with-emacs))

;; Time
(use-package time
  :elpaca nil
  :commands world-clock
  :config
  (setq
   zoneinfo-style-world-list '(("Europe/Berlin" "München"))
   display-time-format "%Y-%m-%d %H:%M"
   display-time-default-load-average nil
   )

  (general-define-key
   :keymaps 'world-clock-mode-map
   :states 'normal
   "q" #'quit-window
   ))

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

(when (eq system-type 'gnu/linux)
  (use-package journalctl
    :elpaca (:host gitlab :repo "bricka/emacs-journalctl")
    :general
    (:prefix my-leader-key
     :states 'normal
     "jj" #'journalctl
     "ju" #'journalctl-user)
    :config
    (general-define-key
     :keymaps 'journalctl-mode-map
     :states 'normal
     "gr" #'journalctl-refresh
     "q" #'quit-window
     "ZZ" #'quit-window)
    ))

(use-package deadgrep)

(use-package ledger-mode
  :mode "\\.ledger\\'" "\\.timedot"
  :config
  (setq ledger-report-resize-window nil)
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
   "mo" #'ledger-occur
   "mcb" #'ledger-mode-clean-buffer
   )

  (add-hook 'ledger-mode-hook (lambda () (setq show-trailing-whitespace t)))

  ;; Reports
  (general-define-key
   :keymaps 'ledger-report-mode-map
   :states 'normal
   "e" #'ledger-report-edit-report
   "q" #'ledger-report-quit)
  )

;; Cannot enabled flycheck-hledger, because ledger-mode seems to not work with "hledger" for me
;; (use-package flycheck-hledger
;;   :after (flycheck ledger-mode)
;;   )

;; Local Configuration
(elpaca-wait)
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
    (find-file local-config-location))
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "fl" #'my/visit-local-config)
  (let ((straight-current-profile 'local))
    (load-file local-config-location)))

(server-start)

(provide 'init)
;;; init.el ends here
