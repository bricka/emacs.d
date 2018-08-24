;;; init.el --- Alex Brick's Emacs configuration

;;; Commentary:
;;;
;;; My Emacs configuration

;;; Code:

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
                tab-width indentation
                web-mode-code-indent-offset indentation
                typescript-indent-level indentation
                js-indent-level indentation
))

(my/set-indentation 2)

(use-package editorconfig
  :ensure t
  :delight
  :config
  (editorconfig-mode 1))

;; Parens
(show-paren-mode 1)
(electric-pair-mode 1)

;; Mode Line
(use-package delight
  :ensure t
  :config
  (delight '((auto-revert-mode nil autorevert)
             (undo-tree-mode nil t)
             (org-indent-mode nil t)
             ))
  )

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
(use-package evil
  :ensure t

  :init

  (setq evil-want-C-u-scroll t)

  :config

  (evil-mode 1)
  (evil-ex-define-cmd "ls" 'helm-mini)
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
  (evil-add-hjkl-bindings messages-buffer-mode-map 'emacs)
  ;; (evil-add-hjkl-bindings docker-images-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'ensime-inspector-mode)
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'docker-images-mode)

  (define-key evil-insert-state-map (kbd "C-v") 'yank)
  )

(use-package evil-magit
  :ensure t
  :after evil magit
  )

(use-package evil-commentary
  :ensure t
  :after evil
  :delight
  :config
  (evil-commentary-mode)
  )

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-org
  :ensure t
  :delight
  :after evil org

  :config
  (setq evil-org-key-theme '(return))
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))

  (evil-define-key 'normal evil-org-mode-map
    (kbd "^") 'evil-org-beginning-of-line
    )
  )

;; Git Configuration
(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends (delq 'Git vc-handled-backends))) ; Disable VC for Git

(use-package git-gutter
  :ensure t
  :delight
  :config
  (global-git-gutter-mode t))

;; Powerline
(use-package spaceline
  :ensure t
  :init (require 'spaceline-config)
  :config
  (spaceline-spacemacs-theme)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  )
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)
;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

;; Theme
;; (load-theme 'spacemacs-dark 1)
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night 1)
  )
;; (load-theme 'sanityinc-tomorrow-night 1)
;; (load-theme 'doom-tomorrow-night 1)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-use-git-grep t)
  (setq projectile-indexing-method "alien")

  (use-package helm-projectile
    :ensure t
    :config
    (setq helm-projectile-set-input-automatically nil)))

;; Helm
(use-package helm
  :pin melpa-stable
  :delight helm-mode
  :ensure t
  :config
  (helm-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  )
;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)

;; Web Mode
(use-package web-mode
  :ensure t

  :mode ("\\.html\\'" "\\.jsx?\\'" "\\.mustache\\'" "\\.php\\'")

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
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook 'enable-tern-mode-for-js)
  )

(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tern)
  )

;; JSON
(use-package json-mode
  :ensure t

  :config
  (add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode)))

;; Typescript

(use-package tide
  :ensure t
  :delight
  :pin melpa-stable

  :config

  (defun my/use-tslint-from-node-modules ()
    "Configure flycheck to use tslint from node_modules."
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (tslint (and root
                        (expand-file-name "node_modules/.bin/tslint"
                                          root))))
      (when tslint
        (setq-local flycheck-typescript-tslint-executable tslint))))

  (defun setup-tide-mode ()
    "Initialize Tide mode."
    (interactive)
    (tide-setup)
    (tide-hl-identifier-mode +1)
    ;; If there is a directory with "package.json" in it, we take that
    ;; as the location from which to search for a tsserver and set
    ;; tide-tsserver-executable to it, if it exists.
    (let* ((package-root (locate-dominating-file default-directory
                                                 "package.json"))
           (path
            (and package-root
                 (expand-file-name "node_modules/typescript/bin/tsserver"
                                   (expand-file-name package-root)))))
      (when (and path
                 (file-exists-p path))
        (make-local-variable 'tide-tsserver-executable)
        (setq tide-tsserver-executable path)
        ))
    )

  (add-hook 'typescript-mode-hook 'setup-tide-mode)
  (add-hook 'typescript-mode-hook 'my/use-tslint-from-node-modules)
  (setq tide-format-options '(:indentSize 2
                              :tabSize 2
                              :convertTabsToSpaces t
                              ))
  )

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

;; Docker
(use-package docker
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

;; Mediawiki
(use-package mediawiki
  :ensure t
  )

;; Which Key
(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode))

;; Company Code Completion
(use-package company
  :ensure t
  :delight

  :defines company-dabbrev-downcase

  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil)
  )

(use-package company-quickhelp
  :ensure t
  :delight
  :after company
  :config
  (company-quickhelp-mode)
  )

;; Rainbow Mode
(use-package rainbow-mode
  :delight
  :ensure t

  :config
  (add-to-list 'rainbow-html-colors-major-mode-list 'less-css-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

;; ENSIME
(use-package ensime
  :ensure t
  :pin melpa-stable

  :config
  (setq ensime-use-helm t))

;; Flycheck

(use-package flycheck
  :ensure t
  :delight
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

;; Latex

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq TeX-PDF-mode t)

;; (add-hook 'latex-mode-hook 'TeX-source-correlate-mode)

;; LESS

(use-package less-css-mode
  :ensure t)

;; Nagios
(add-to-list 'load-path "c:/Users/abrick/.emacs.d/modes")
(autoload 'nagios-mode "nagios-mode" nil t)
(add-to-list 'auto-mode-alist '("nagios.*\\.cfg\\'" . nagios-mode))

;; nginx
(use-package nginx-mode
  :ensure t)

;; Org mode
(defun my/enable-org-mode-wordwrap ()
  "Enables word-wrapping for org mode."
  (visual-line-mode)
  (org-indent-mode))

(add-hook 'org-mode-hook 'my/enable-org-mode-wordwrap)
(setq org-startup-folded "showall")
(setq org-special-ctrl-a/e t)

;; kubernetes
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;; Plant UML
(use-package plantuml-mode
  :ensure t
  )

;; Graphviz
(use-package graphviz-dot-mode
  :ensure t
  )

;; Python

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(use-package company-anaconda
  :ensure t
  :after anaconda-mode company
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )

(use-package string-inflection
  :ensure t
  )

;; C++

(use-package modern-cpp-font-lock
  :ensure t
  :delight
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  )

(use-package ggtags
  :ensure t

  :config

  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (ggtags-mode 1))))
  )

(use-package cmake-mode
  :ensure t
  )

;; Java

(use-package meghanada
  :ensure t
  :config
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)))
  (add-to-list 'company-backends 'company-meghanada)
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "c:/Users/abrick/apache-maven-3.5.4/bin/mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))
  )

(defun my/java-indent-setup ()
  "Define preferred indentation style."
  (progn
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
  ))
(add-hook 'java-mode-hook 'my/java-indent-setup)

;; XML

(setq nxml-slash-auto-complete-flag t)

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

(defun my/revert-buffer ()
  "Revert buffer correctly."
  (interactive)
  (if (equal system-type 'windows-nt)
      (revert-buffer-with-coding-system 'utf-8-dos)
      (revert-buffer)
  ))

(set-group-string "b" "Buffers")
(evil-leader/set-key
  "bb" 'helm-mini
  "bd" 'kill-this-buffer
  "bD" 'kill-other-buffers
  "br" 'my/revert-buffer)

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
  "fr" 'rename-file-and-buffer
  )

;; Git Keys
(defun my/delete-merged-branches ()
  "Delete all branches that have been merged into master."
  (interactive)
  (let ((branches (magit-git-lines "branch --merged master")))
    (dolist (branch branches)
      (if (not (equal branch "master"))
          (magit-call-git (concat "branch -d " branch)))))
  )

(set-group-string "g" "Git")
(evil-leader/set-key
  "gb" 'magit-branch-popup
  "gc" 'magit-commit-popup
  "gd" 'magit-diff-popup
  "gD" 'my/delete-merged-branches
  "gp" 'magit-push-popup
  "gr" 'magit-rebase-popup
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

;; Text Keys
(set-group-string "t" "Text")
(evil-leader/set-key
  "t_" 'string-inflection-underscore
  "tC" 'string-inflection-camelcase)

;; Window Keys
(set-group-string "w" "Window")
(evil-leader/set-key
  "w-" 'split-window-below
  "w/" 'split-window-right
  "w=" 'balance-windows
  "wn" 'make-frame)

;;; MAJOR MODE KEYS
(load-file "~/.emacs.d/major-mode.el")

;; Other Keys
(global-set-key (kbd "s-u") 'revert-buffer-no-confirm)

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

(add-to-list 'load-path "c:/cygwin64/bin")

;; Swagger

(define-derived-mode swagger-yaml-mode yaml-mode
  "Swagger YAML"
  "Major mode for Swagger YAML files"
  )
(add-to-list 'auto-mode-alist '("swagger.\\.ya?ml\\'" . swagger-yaml-mode))

(flycheck-define-checker swagger
  "A syntax checker for Swagger using swagger-cli."

  :command ("C:/Users/abrick/AppData/Roaming/npm/swagger" "validate" source)

  :error-patterns (
   (error line-start (message) "at line " line ", column " column ":" line-end)
   (error line-start "SyntaxError: " (message) line-end)
   )

  :error-filter
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        (unless (flycheck-error-line err)
          (setf (flycheck-error-line err) 1)))
      errors))

  :modes (swagger-yaml-mode)
  )

(add-to-list 'flycheck-checkers 'swagger)


;; (use-package openapi-yaml-mode
;;   :ensure t
;;   :mode "swagger\\.yaml\\'"
;;   )

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
    (meghanada openapi-yaml-mode cmake-mode ggtags modern-cpp-font-lock rtags company-quickhelp string-inflection graphviz-dot-mode elpy ample-theme doom-themes solarized-theme editorconfig js2-mode tide mediawiki edit-server nginx-mode dockerfile-mode nagios-mode delight rainbow-delimiters evil-surround git-gutter-fringe diff-hl rainbow-mode less-css-mode web-mode json-mode jsdon-mode spaceline-config evil-magit use-package helm monokai-theme moe-theme color-theme-sanityinc-tomorrow zenburn-theme spaceline powerline flx-ido projectile magit evil)))
 '(safe-local-variable-values
   (quote
    ((eval my/set-indentation 2)
     (typescript-indent-level . 2)
     (typesecript-indent-level . 2)
     (standard-indent . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
