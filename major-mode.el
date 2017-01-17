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
