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

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(cond ((eq system-type 'darwin) (set-face-attribute 'default nil :font "Menlo 15"))
      (t (set-face-attribute 'default nil :font "DejaVu Sans Mono 15")))

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

;; Evil
(use-package evil
  :init

  (setq evil-want-C-u-scroll t)

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

(use-package evil-magit
  :after evil magit
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
   "K" 'org-metaup
   "J" 'org-metadown

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
   )
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))) ; Disable VC for Git

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

;; Mode line
(use-package doom-modeline
  :after all-the-icons
  :defines doom-modeline-buffer-encoding doom-modeline-checker-simple-format doom-modeline-minor-modes doom-modeline-mu4e
  :config
  (setq
   doom-modeline-buffer-encoding nil
   doom-modeline-checker-simple-format nil
   doom-modeline-minor-modes nil
   doom-modeline-mu4e t
   )
  (doom-modeline-mode 1))

;; Theme
(use-package modus-vivendi-theme
  :defines modus-vivendi-theme-bold-constructs modus-vivendi-theme-rainbow-headings
  :config
  (setq
   modus-vivendi-theme-rainbow-headings t
   modus-vivendi-theme-bold-constructs t
   )
  (load-theme 'modus-vivendi t)
  )

;; Projectile
(use-package projectile
  :config
  ; Not using general to avoid deferring
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "p'" 'visit-term-projectile-root
   "pi" 'projectile-invalidate-cache
   )
  (projectile-mode)
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'ivy)
  )

;; Helm
(use-package helm
  :after helm-icons
  :blackout
  :config
  ;; Not using :general to avoid deferring
  (general-define-key
   "M-x" 'helm-M-x
   )
  (general-define-key
   :states 'normal
   :prefix my-leader-key
   "bb" 'helm-buffers-list
   "ha" 'helm-apropos
   )
  (bind-key "M-x" 'helm-M-x)
  (helm-mode 1)
  )

(use-package helm-ag
  :after helm)

(use-package helm-projectile
  :after helm helm-ag
  :general
  (:states 'normal
   :prefix my-leader-key
   "pf" 'helm-projectile-find-file
   "pl" 'helm-projectile-switch-project
   "sp" 'helm-projectile-ag
   )
  )

(use-package helm-icons
  :config
  (helm-icons-enable)
  )

(use-package helm-flx
  :config
  (helm-flx-mode 1)
  )

(use-package treemacs
  :commands treemacs
  :general
  (:states 'normal
   :prefix my-leader-key
   "T" 'treemacs
   )
  :config
  (setq
   treemacs-wrap-around nil
   )
  (treemacs-follow-mode)
  )

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons")
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
  :after lsp-mode flycheck
  :mode "\\.php\\'"
  :config
  (flycheck-add-next-checker 'lsp 'php)
  )

(use-package my-flycheck-phpstan
  :straight (:local-repo "my-flycheck-phpstan")
  :after php-mode
  :config
  (flycheck-add-next-checker 'phpstan 'php-phpcs)
  (flycheck-add-next-checker 'phpstan 'php-phpmd)
  (setq-default phpstan-level nil)
  )

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
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
  (setq company-dabbrev-downcase nil)
  )

;; Rainbow Mode
(use-package rainbow-mode
  :blackout
  :hook css-mode

  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  )

;; LSP
(use-package lsp-mode
  :after flycheck
  :hook (
         (java-mode . lsp)
         (php-mode . lsp)
         (scala-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (yaml-mode . lsp)
         )
  :general
  (:states 'normal
   :keymaps '(
              java-mode-map
              php-mode-map
              scala-mode-map
              typescript-mode-map
              web-mode-map
              )
   :prefix my-leader-key
   "mi" 'lsp-ui-doc-glance
   "mr" 'lsp-find-references
   )
  )

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)
  )

(use-package lsp-java
  :after lsp-mode
  )

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-enable nil)
  )

;; Flycheck

(use-package flycheck
  :blackout
  :general
  (:states 'normal
   :prefix my-leader-key
   "el" 'flycheck-list-errors
   "en" 'flycheck-next-error
   "ep" 'flycheck-previous-error
   "ev" 'flycheck-verify-setup
   )
  :config
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
 "mo" 'dired-omit-mode
 )

;; ELisp
(general-define-key
 :prefix my-leader-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 "mn" 'narrow-to-defun
 "mx" '(:ignore t :wk "Eval")
 "mxd" 'eval-defun
 "mxe" 'eval-expression
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
   "m*" 'org-toggle-heading
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
   "mx" 'org-toggle-checkbox

   "ms" '(:ignore t :wk "Search")
   "msh" 'helm-org-in-buffer-headings

   "mT" '(:ignore t :wk "Tables")
   "mT*" 'org-table-recalculate
   "mTt" 'org-table-iterate
   "mTI" 'org-table-iterate-buffer-tables

   "mTd" '(:ignore t :wk "Delete")
   "mTdr" 'org-table-kill-row
   )
  :config
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq
   org-directory "~/org"
   org-default-notes-file "~/Dropbox/Arbeit Sync/org/work.org"
   org-startup-indented t
   org-startup-folded t
   org-special-ctrl-a/e t
   org-hide-emphasis-markers t
   org-return-follows-link t
   )

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (add-hook 'org-mode-hook 'my/enable-org-mode-wordwrap)

  (require 'ox-md)

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
  (setq org-superstar-item-bullet-alist '(
                                          (?- . ?➤)
                                          ))
  )

(use-package gnuplot)

(use-package my-org-agenda-notifier
  :straight (:local-repo "my-org-agenda-notifier")
  :after org
  :config
  (my-org-agenda-notifier-mode)
  )

(use-package helm-org
  :after helm org
  :commands helm-org-in-buffer-headings
  )

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
  :commands display-time-world
  :config
  (setq
   zoneinfo-style-world-list '(
                               ("Europe/Berlin" "München")
                               ("Europe/Moscow" "Moscow")
                               ("America/Sao_Paulo" "Belo Horizonte")
                               )
   display-time-format "%Y-%m-%d %H:%M"
   display-time-default-load-average nil
   )
  )

;; Reddit
(use-package md4rd
  :commands md4rd md4rd-login
  :defines md4rd-subs-active
  :config
  (add-to-list 'evil-emacs-state-modes 'md4rd-mode)
  (setq
   md4rd-subs-active '(awww germany))
  )

;; Local Configuration
(if (file-exists-p (expand-file-name "~/.emacs.d/local-config.el"))
    (load-file (expand-file-name "~/.emacs.d/local-config.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "4a7abcca7cfa2ccdf4d7804f1162dd0353ce766b1277e8ee2ac7ee27bfbb408f" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (org-timeline org-super-agenda yaml-mode lsp-treemacs company-lsp lsp-company lsp-ui lsp-mode ox-md org-wild-notifier calfw calfw-org unicode-fonts mu4e-alert org-bullets dashboard ivy-posframe all-the-icons-ivy ivy-rich counsel-projectile evil-visualstar exec-path-from-shell 0x0 evil-magit ox-mediawiki gnuplot feature-mode org-jira ox-jira meghanada openapi-yaml-mode cmake-mode ggtags modern-cpp-font-lock rtags company-quickhelp string-inflection graphviz-dot-mode elpy ample-theme doom-themes solarized-theme editorconfig js2-mode tide mediawiki edit-server nginx-mode dockerfile-mode nagios-mode delight rainbow-delimiters evil-surround git-gutter-fringe diff-hl rainbow-mode less-css-mode web-mode json-mode jsdon-mode spaceline-config use-package helm monokai-theme moe-theme color-theme-sanityinc-tomorrow zenburn-theme spaceline powerline flx-ido projectile magit evil)))
 '(safe-local-variable-values
   (quote
    ((json-encoding-default-indeitation . "    ")
     (standard-indent . 4)
     (tide-tsserver-executable . "/home/abrick/src/core/frontend/node_modules/.bin/tsserver")
     (projectile-enable-caching . t)
     (tide-tsserver-executable . "frontend/node_modules/bin/tsserver")
     (flycheck-php-phpcs-executable . "/home/abrick/bin/phpcs-core")
     (eval my/set-indentation 2)
     (typescript-indent-level . 2)
     (typesecript-indent-level . 2)
     (standard-indent . 2)
     (flycheck-php-phpcs-executable . "/home/abrick/bin/phpcs-core")
     (flycheck-php-phpcs-executable . "/home/abrick/bin/phpcs-siemens")
     (flycheck-php-phpmd-executable . "/home/abrick/bin/phpmd-core")
     (tide-tsserver-executable . "frontend/node_modules/.bin/tsserver")
     (flycheck-typescript-tslint-executable . "/home/abrick/src/core/frontend/node_modules/.bin/tslint")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
