(defun set-group-string-for-mode (mode prefix title)
  "Set the which-key string in major mode MODE for LEADER PREFIX to TITLE."
  (which-key-add-major-mode-key-based-replacements mode
    (concat evil-leader/leader " " prefix) title))

;; LaTeX
(set-group-string-for-mode 'latex-mode "m" "LaTeX")
(evil-leader/set-key-for-mode 'latex-mode
  "mm" 'TeX-command-run-all
  )

;; Scala
(defun maven-project-p ()
  "Determine if this project is a Maven project."
  (file-exists-p (concat (projectile-project-root) "/pom.xml")))

(defun sbt-project-p ()
  "Determine if this project is an SBT project."
  (file-exists-p (concat (projectile-project-root) "/build.sbt")))

(defun ensime-gen ()
  "Generate the ensime configuration file."
  (interactive)
  (cond ((maven-project-p) (projectile-run-async-shell-command "mvn ensime:generate"))
        ((sbt-project-p) (sbt:command "ensimeConfig"))
        ))
(set-group-string-for-mode 'scala-mode "m" "Scala")
(evil-leader/set-key-for-mode 'scala-mode
  "el" 'ensime-show-all-errors-and-warnings ; Overriding flycheck for Scala
  "mm" 'ensime-gen
  "mi" 'ensime-inspect-type-at-point
  "ms" 'ensime-search
  )

(set-group-string-for-mode 'scala-mode "mg" "Goto")
(evil-leader/set-key-for-mode 'scala-mode
  "mgi" 'ensime-goto-impl
  "mgt" 'ensime-goto-test
  )

;; org-mode
(set-group-string-for-mode 'org-mode "m" "Org")
(evil-leader/set-key-for-mode 'org-mode
  "m*" 'org-toggle-heading
  "m-" 'org-toggle-item
  "ma" 'org-archive-subtree
  "mc" 'org-cycle
  "mC" 'org-global-cycle
  "md" 'org-deadline
  "me" 'org-export-dispatch
  "mE" 'outline-show-all
  "mp" 'org-priority
  "ms" 'org-schedule
  "mt" 'org-todo
  "mx" 'org-toggle-checkbox
  )

(set-group-string-for-mode 'org-mode "mT" "Tables")
(evil-leader/set-key-for-mode 'org-mode
  "mT*" 'org-table-recalculate
  "mTt" 'org-table-iterate
  "mTI" 'org-table-iterate-buffer-tables
  )

(set-group-string-for-mode 'org-mode "mTd" "Delete")
(evil-leader/set-key-for-mode 'org-mode
  "mTdr" 'org-table-kill-row
  )

;; (defun my/add-below-org-mode-same-level ()
;;   (interactive)
;;   (progn
;;     (org-insert-heading-after-current)
;;     (evil-append-line 1)))

;; (defun my/add-above-org-mode-same-level ()
;;   (interactive)
;;   (progn
;;     (org-insert-heading)
;;     (evil-append-line 1)))

;; (evil-define-key 'normal org-mode-map
;;   ">>" 'org-do-demote
;;   "<<" 'org-do-promote
;;   (kbd "C-o") 'my/add-below-org-mode-same-level
;;   (kbd "C-O" )'my/add-above-org-mode-same-level
;;   )

;; dired-mode

(set-group-string-for-mode 'dired-mode "m" "Dired")
(evil-leader/set-key-for-mode 'dired-mode
  "mo" 'dired-omit-mode)

;; json-mode

(set-group-string-for-mode 'json-mode "m" "JSON")
(evil-leader/set-key-for-mode 'json-mode
  "mB" 'json-pretty-print-buffer
  "mb" 'json-pretty-print
  )

;; typescript mode
(set-group-string-for-mode 'typescript-mode "m" "Typescript")
(set-group-string-for-mode 'typescript-mode "mg" "Goto")
(evil-leader/set-key-for-mode 'typescript-mode
  "mi" 'tide-documentation-at-point
  "mr" 'tide-references
  "mgd" 'tide-jump-to-definition
  "mgt" (lambda () (tide-jump-to-definition 1))
  "mr" 'tide-restart-server
  )

;; Kubernetes configs

(set-group-string-for-mode 'kubernetes-mode "m" "Kubernetes")
(evil-leader/set-key-for-mode 'kubernetes-mode
  "mr" 'kubernetes-refresh
  )

;; PlantUML

(defun my/plantuml-preview-new-window ()
  "Preview the PlantUML in a new window."
  (interactive)
  (plantuml-preview 4))

(set-group-string-for-mode 'plantuml-mode "m" "PlantUML")
(evil-leader/set-key-for-mode 'plantuml-mode
  "mp" 'my/plantuml-preview-new-window
  )

;; Graphviz
(set-group-string-for-mode 'graphviz-dot-mode "m" "Graphviz")
(evil-leader/set-key-for-mode 'graphviz-dot-mode
  "mp" 'graphviz-dot-preview
  )

;; C
(set-group-string-for-mode 'c-mode "m" "C")
(evil-leader/set-key-for-mode 'c-mode
  "mgd" 'ggtags-find-definition
  "mr" 'ggtags-find-reference
  )

;; C++
(set-group-string-for-mode 'c++-mode "m" "C++")
(evil-leader/set-key-for-mode 'c++-mode
  "mgd" 'ggtags-find-definition
  "mr" 'ggtags-find-reference
  )

;; Python
(set-group-string-for-mode 'python-mode "m" "Python")
(evil-leader/set-key-for-mode 'python-mode
  "mgd" 'anaconda-mode-find-definitions
  )

;; Java
(set-group-string-for-mode 'java-mode "m" "Java")
(evil-leader/set-key-for-mode 'java-mode
  "mi" 'meghanada-typeinfo
  "mI" 'meghanada-import-at-point
  "mr" 'meghanada-restart
  )

(set-group-string-for-mode 'java-mode "mg" "Goto")
(evil-leader/set-key-for-mode 'java-mode
  "mgd" 'meghanada-jump-declaration
  "mgr" 'meghanada-reference
  )

;; PHP
(set-group-string-for-mode 'php-mode "m" "PHP")

(set-group-string-for-mode 'php-mode "mg" "Goto")
(evil-leader/set-key-for-mode 'php-mode
  "mgd" 'ac-php-find-symbol-at-point
  )
