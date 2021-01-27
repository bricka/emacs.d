;;; init.el --- Alex Brick's Emacs configuration

;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

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
(setq ring-bell-function 'ignore)
(setq custom-file "/dev/null")
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(cond ((eq system-type 'darwin) (set-face-attribute 'default nil :font "Menlo 15"))
      (t (set-face-attribute 'default nil :font "DejaVu Sans Mono 15")))

(set-face-attribute 'variable-pitch nil :font "DejaVu Serif 15")

;; Indentation
(defun my/set-indentation (indentation)
  "Set the indentation level to INDENTATION."
  (setq-default standard-indent indentation
                tab-width indentation
                web-mode-code-indent-offset indentation
                typescript-indent-level indentation
                js-indent-level indentation
                plantuml-indent-level indentation
))

(my/set-indentation 2)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
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
   "'" 'visit-term-buffer
   "d" 'dired-open-current-directory

   "b" '(:ignore t :wk "Buffers")
   "bd" 'kill-this-buffer
   "bD" 'my/kill-other-buffers
   "br" 'revert-buffer

   "e" '(:ignore t :wk "Errors")

   "f" '(:ignore t :wk "Files")

   "g" '(:ignore t :wk "Git")
   "gh" '(:ignore t :wk "Hunks")

   "h" '(:ignore t :wk "Help")
   "hf" 'describe-function
   "hk" 'describe-key
   "hm" 'describe-mode
   "hv" 'describe-variable

   "m" '(:ignore t :wk "Major")

   "p" '(:ignore t :wk "Project")

   "s" '(:ignore t :wk "Search")

   "w" '(:ignore t :wk "Window")
   )
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
    (exec-path-from-shell-initialize)
    )
  )

(use-package all-the-icons)

(use-package alert
  :config
  (setq alert-default-style 'libnotify)
  (setq alert-fade-time 20)
  (setq alert-persist-idle-time 60)
  )

;; Startup

(use-package dashboard
  :after all-the-icons projectile
  :config
  ; Not using :general to avoid deferring the loading
  (general-define-key
   :states 'normal
   :keymaps 'dashboard-mode-map
   "{" 'dashboard-previous-section
   "}" 'dashboard-next-section
   "r" 'dashboard-refresh-buffer
   )
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '(
                          (recents . 5)
                          (projects . 5)
                          (agenda . 5)
                          ))
  (dashboard-setup-startup-hook)
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
  (add-to-list 'auto-insert-alist '(php-mode . "php-template.php"))
  (add-hook 'find-file-hook 'auto-insert)
  )

;; Undo
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

;; Evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-tree)

  :config

  (evil-mode 1)
  (let ((maps '(
                archive-mode-map
                messages-buffer-mode-map
                )))
    (dolist (map maps)
      (evil-add-hjkl-bindings map 'emacs)))

  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  (add-to-list 'evil-emacs-state-modes 'display-time-world-mode)
  (evil-set-initial-state 'org-agenda-mode 'motion)

  (general-define-key
   :states 'insert
   "C-v" 'yank
   )
  (general-define-key
   :states 'normal
   "C-u" 'evil-scroll-up
   )
  )

(use-package evil-collection
  :defines evil-collection-mode-list evil-collection-want-unimpaired-p
  :after evil
  :config
  (setq evil-collection-want-unimpaired-p nil)
  (setq evil-collection-mode-list
        '(
          dired
          magit
          )
        )
  (evil-collection-init)
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
   "<return>" 'evil-org-return
   )
  (:states 'normal
   :keymaps 'org-mode-map
   ;; Motion
   "^" 'evil-org-beginning-of-line
   "$" 'evil-org-end-of-line

   ;; Inserting
   "A" 'evil-org-append-line
   "I" 'evil-org-insert-line
   "o" 'evil-org-open-below
   "O" 'evil-org-open-above

   ;; Move
   "C-K" 'org-metaup
   "C-J" 'org-metadown

   ;; Delete
   "d" 'evil-org-delete
   "x" 'evil-org-delete-char

   ;; Follow
   "<return>" 'org-open-at-point
   )
  (:states '(normal visual)
   :keymaps 'org-mode-map
   "<" 'evil-org-<
   ">" 'evil-org->
   )

  ;; Text Objects
  (:states '(visual operator)
   :keymaps 'org-mode-map
   "ae" 'evil-org-an-object
   "ie" 'evil-org-inner-object
   )

  (:states 'motion
   :keymaps 'org-agenda-mode-map
   "<tab>" 'org-agenda-goto
   "<return>" 'org-agenda-switch-to
   "," 'org-agenda-schedule
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "H" 'org-agenda-do-date-earlier
   "L" 'org-agenda-do-date-later
   "t" 'org-agenda-todo
   "u" 'org-agenda-undo
   "[" 'org-agenda-earlier
   "]" 'org-agenda-later
   ">" 'org-agenda-goto-date
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
   "gb" 'magit-branch
   "gB" 'magit-blame
   "gc" 'magit-commit
   "gd" 'magit-diff
   "gp" 'magit-push
   "gr" 'magit-rebase
   "gs" 'magit-status

   "gll" 'magit-log
   "glf" 'magit-log-buffer-file
   )
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends)) ; Disable VC for Git
  )

(use-package git-gutter
  :blackout
  :config
  ; Not using :general to avoid deferring
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "ghn" 'git-gutter:next-hunk
   "ghp" 'git-gutter:previous-hunk
   )
  (global-git-gutter-mode t)
  )

(use-package browse-at-remote
  :general
  (:states '(normal visual)
   :prefix my-leader-key
   "gp" 'browse-at-remote
   )
  :config
  (setq browse-at-remote-prefer-symbolic nil)
  )

(use-package transient)

(use-package forge
  :after magit
  )

;; Mode line
(use-package doom-modeline
  :after all-the-icons
  :defines doom-modeline-buffer-encoding doom-modeline-buffer-file-name-style doom-modeline-checker-simple-format doom-modeline-minor-modes doom-modeline-mu4e
  :config
  (setq
   doom-modeline-buffer-encoding nil
   doom-modeline-checker-simple-format nil
   doom-modeline-minor-modes nil
   doom-modeline-buffer-file-name-style 'truncate-with-project
   doom-modeline-mu4e t
   )
  (doom-modeline-mode 1))

;; Theme
(use-package modus-themes
  :straight (:branch "main")
  :config
  (setq modus-themes-headings
        '((t . rainbow)))
  (setq modus-themes-bold-constructs t)
  (load-theme 'modus-vivendi t)
  )

;; Window
(use-package zoom
  :config
  (zoom-mode 1)
  )

;; Whitespace
(use-package origami
  :hook (prog-mode . origami-mode)
  :general
  (:states 'normal
   :keymaps 'origami-mode-map
   "<tab>" 'origami-recursively-toggle-node
   )
  )

;; Projectile
(use-package projectile
  :config
  ;; Not using general to avoid deferring
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "p'" 'visit-term-projectile-root
   "pi" 'projectile-invalidate-cache
   "pK" 'projectile-kill-buffers
   )
  (projectile-mode)
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy)
  )

;; Ivy
(use-package counsel
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "bb" 'ivy-switch-buffer
   )
  (general-define-key
   "M-x" 'counsel-M-x
   )
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-mode 1)
  )

(use-package ivy-rich
  :after counsel
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1)
  )

(use-package all-the-icons-ivy-rich
  :after ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1)
  )

(use-package counsel-projectile
  :after counsel projectile
  :config
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "pb" 'counsel-projectile-switch-to-buffer
   "pl" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file
   "sp" 'counsel-projectile-ag
   )
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
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-magit
  :after treemacs magit
  )

;; Web Mode
(use-package web-mode
  :mode ("\\.html\\'" "\\.jsx?\\'" "\\.mustache\\'" "\\.tsx\\'")

  :config

  (defvar web-mode-js-syntax-table)

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
        (setq web-mode-js-syntax-table st)
        (add-hook 'web-mode-hook
                  (lambda () (if (my/web-mode-is-js) (set-syntax-table web-mode-js-syntax-table))))
        )))

  (my/create-web-mode-js-syntax-table)
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]s[x]?\\'")))
  (add-hook 'web-mode-hook 'enable-tern-mode-for-js)
  )

;; JSON
(use-package json-mode
  :mode ("\\.json" "\\.babelrc\\'" "\\.eslintrc\\'")
  :general
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'json-mode-map
   "mb" 'json-pretty-print-buffer
   )
  )

;; PHP

(use-package php-mode
  :mode "\\.php\\'"
  )

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
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

(add-to-list 'auto-mode-alist '("\/\\.env" . conf-mode))

;; Which Key
(use-package which-key
  :blackout
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :blackout
  :defines company-dabbrev-downcase
  :hook (after-init . global-company-mode)

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
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  )

;; LSP
(use-package lsp-mode
  :hook (
         (java-mode . lsp)
         (php-mode . lsp)
         (rustic-mode . lsp)
         (scala-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         )
  :general
  (:states 'normal
   :keymaps '(
              java-mode-map
              php-mode-map
              rustic-mode-map
              scala-mode-map
              sh-mode-map
              typescript-mode-map
              web-mode-map
              )
   :prefix my-leader-key
   "mE" 'lsp-treemacs-errors-list
   "mi" 'lsp-ui-doc-glance
   "mR" 'lsp-rename
   "mx" 'lsp-execute-code-action
   )
  :config

  (setq lsp-headerline-breadcrumb-enable nil)
  )

(use-package lsp-java
  )

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-enable nil)
  )

(use-package lsp-treemacs
  :after treemacs
  :general
  (:states 'normal
   :keymaps '(
              java-mode-map
              php-mode-map
              scala-mode-map
              sh-mode-map
              typescript-mode-map
              web-mode-map
              )
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
  (global-flycheck-mode)

  (defun my/use-eslint-from-node-modules ()
    "Configure Flycheck to use eslint from node_modules if present."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (global-eslint (executable-find "eslint"))
           (local-eslint (expand-file-name "node_modules/.bin/eslint.cmd"
                                           root))
           (eslint (if (file-executable-p local-eslint)
                       local-eslint
                     global-eslint)))
      (setq-local flycheck-javascript-eslint-executable eslint)))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  )

;; Shell
(load-file "~/.emacs.d/shell-configuration.el")

;; Dired

(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\...+$"))

(defun dired-open-current-directory ()
  "Open dired in the directory of this file."
  (interactive)
  (dired (file-name-directory (buffer-file-name))))

(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'dired-mode-map
 "br" 'revert-buffer
 "mo" 'dired-omit-mode
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

;; Plain Text
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'text-mode-map
 "mTj" 'table-justify
 "mTs" 'table-span-cell
 "mTS" 'table-split-cell
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

(defun my-daily-agenda ()
  "Open up daily agenda."
  (interactive)
  (org-agenda-list nil nil 'day)
  )

(use-package org
  :straight org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :general
  (:states 'normal
   :prefix my-leader-key
   "a" 'my-daily-agenda
   "A" 'org-agenda
   "c" 'org-capture
   )
  (:states 'normal
   :prefix my-leader-key
   :keymaps 'org-mode-map
   "m." 'org-time-stamp
   "m," 'org-schedule
   "m-" 'org-toggle-item
   "m/" 'org-sparse-tree
   "ma" 'org-archive-subtree
   "mc" 'org-cycle
   "mC" 'org-global-cycle
   "md" 'org-deadline
   "me" 'org-export-dispatch
   "mE" 'outline-show-all
   "mn" 'org-narrow-to-subtree
   "mp" 'org-priority
   "mr" 'org-reveal
   "mt" 'org-todo

   "ms" '(:ignore t :wk "Search")
   "msh" 'helm-org-in-buffer-headings

   "mT" '(:ignore t :wk "Tables")
   "mT*" 'org-table-recalculate
   "mTt" 'org-table-iterate
   "mTI" 'org-table-iterate-buffer-tables

   "mTd" '(:ignore t :wk "Delete")
   "mTdr" 'org-table-kill-row
   )
  (:states '(normal visual)
   :keymaps 'org-mode-map
   "m*" 'org-toggle-heading
   "mx" 'org-toggle-checkbox
   )
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; Font Configuration
  (set-face-attribute 'org-checkbox nil :family "monospace")
  (set-face-attribute 'org-checkbox-statistics-todo nil :family "monospace")
  (set-face-attribute 'org-checkbox-statistics-done nil :family "monospace")
  (set-face-attribute 'org-link nil :family "monospace")
  (set-face-attribute 'org-column-title nil :family "monospace")

  (setq
   org-directory "~/org"
   org-startup-indented t
   org-startup-folded t
   org-special-ctrl-a/e t
   org-hide-emphasis-markers t
   org-return-follows-link t
   org-refile-use-outline-path t
   )
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (add-hook 'org-mode-hook 'my/enable-org-mode-wordwrap)

  ;; Export
  (require 'ox-md)
  (setq
   org-html-validation-link nil
   org-export-with-toc nil
   )

  ;; Agenda
  (setq org-agenda-files '("~/org/"))

  (setq
   org-deadline-warning-days 3
   org-agenda-start-on-weekday nil
   org-agenda-window-setup 'other-window
   )

  (setq org-agenda-time-grid '((daily today require-timed)
                               (1000 1200 1400 1600)
                               "......"
                               "----------------"))
  (setq org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")

  )

(use-package org-super-agenda
  :defines org-super-agenda-header-map org-super-agenda-groups
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map nil)
  (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:discard (:heading-regexp "^<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\} [[:alpha:]]\\{2\\}>$")) ; Date-based notes (e.g. 1-1)
         (:name "Todos" :todo "TODO")
         (:name "Calendar"  ; Optionally specify section name
                :and (:time-grid t :not (:todo t)))
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         ))
  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :defines org-superstar-item-bullet-alist
  :config
  (set-face-attribute 'org-superstar-item nil :family "monospace")
  (set-face-attribute 'org-superstar-header-bullet nil :family "monospace")
  (setq org-superstar-item-bullet-alist '(
                                          (?- . ?➤)
                                          ))
  )

(use-package gnuplot)

(use-package calfw)
(use-package calfw-org
  :after calfw
  :general
  (:states 'normal
   :prefix my-leader-key
   "C" 'cfw:open-org-calendar
   )
  )

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
   "mp" 'my/plantuml-preview-new-window
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
   "mp" 'graphviz-dot-preview
   )
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

(use-package string-inflection
  :general
  (:states 'normal
   :prefix my-leader-key
   "t" '(:ignore t :wk "Text")
   "t_" 'string-inflection-underscore
   "tC" 'string-inflection-camelcase
   )
  )

;; Java

(defun my/java-indent-setup ()
  "Define preferred indentation style."
  (progn
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
  ))
(add-hook 'java-mode-hook 'my/java-indent-setup)

;; Scala

(use-package scala-mode
  :mode "\\.scala\\'")

;; Rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :general
  (states 'normal
   :prefix my-leader-key
   :keymaps 'rustic-mode-map
   "mt" 'rustic-cargo-test)
  :config
  (setq
   lsp-rust-analyzer-cargo-watch-command "clippy"
   )
  )

;; TypeScript

(use-package add-node-modules-path
  :hook (typescript-mode)
  )

;; XML

(use-package nxml-mode
  :straight (:type built-in)
  :mode "\\.xml"
  :config
  (setq nxml-slash-auto-complete-flag t)
  )

;; Kotlin
(use-package kotlin-mode
  :mode "\\.kt\\'"
  )

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
 "fd" 'my/delete-file-and-buffer
 "fe" 'my/visit-emacs-init
 "fm" 'my/move-buffer-file
 "fr" 'my/rename-file-and-buffer
 )

;; Window Keys
(general-define-key
 :prefix my-leader-key
 :states 'normal
 "w-" 'split-window-below
 "w/" 'split-window-right
 "w=" 'balance-windows
 "wn" 'make-frame
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

;; Windows Support

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "c:/cygwin64")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(defun dos2unix ()
  "Convert the line endings in this file from Windows to UNIX."
  (interactive)
  (while (search-forward "
" nil t)
    (replace-match nil nil t))
  )

;; Time
(use-package time
  :config
  (setq
   zoneinfo-style-world-list '(("Europe/Berlin" "München"))
   display-time-format "%Y-%m-%d %H:%M"
   display-time-default-load-average nil
   )
  (display-time-mode)
  )

;; Local Configuration
(if (file-exists-p (expand-file-name "~/.emacs.d/local-config.el"))
    (let ((straight-current-profile 'local))
      (load-file (expand-file-name "~/.emacs.d/local-config.el"))
      ))
