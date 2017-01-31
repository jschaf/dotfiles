;;; tlp.el -- Project management in Emacs

(defun tlp-start-work ()
  "Prompts for TLP project to open.")

(defun tlp--find-available-projects ()
  "Return a list of available projects in `org-agenda-files'."
  ;; Loop over org agenda files
  ;; for each file
  ;; return all headlines with a TLP tag.

  )

(defvar tlp-okrs
  '("emailAddresses"
    "rtfMigration"
    "shipCode"
    "jsReadability"
    "googlejs"
    "fastWalrus"
    "miniProfiler"
    "ajdMigration"
    "allLints"
    "ngExport"
    "surveyRefactor"
    "pythonReadability"
    "es6Migration"
    "googlejsGithub")
  "OKR org-mode tags.")

(defvar tlp-project-tags
  `((name . "Project Tags")
    (candidates . ,tlp-okrs)
    (action . identity))
  "Org mode tags for projects.")

(defun tlp-choose-project-tag ()
  "Choose a project tag."
  (interactive)
  (helm :sources '(tlp-project-tags)))

(defun tlp-set-project-org-tag ()
  "Sets the current headlines tag."
  (interactive)
  (org-toggle-tag (tlp-choose-project-tag))
  ;; Align tags
  (org-set-tags-command nil 'align))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  ";" 'tlp-set-project-org-tag)


(provide 'tlp)
