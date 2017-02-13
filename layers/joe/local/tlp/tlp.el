;;; tlp.el -- Project management in Emacs
(defvar tlp--org-tag-match-expression "tlp"
  "The match expression to find project headings.")


(defvar tlp--org-tag-match-expression-config "tlpConfig"
  "The match expression to find project config headings.")

(define-error 'tlp-error "TLP Error")
(define-error 'tlp-config-format "Bad JSON config" 'tlp-error)
(define-error 'tlp-missing-config "Can't find config" 'tlp-error)

;;;###autoload
(defun tlp-start-work ()
  "Prompts for TLP project to open."
  (interactive)
  (tlp-helm-available-projects)
  (tlp--reset-config)
  (-when-let (json-config (tlp--load-config))
    (tlp-make-config json-config))
  )

;;;###autoload
(defun tlp-helm-available-projects ()
  "Return a list of available projects in `org-agenda-files'."
  (interactive)
  (helm :sources (helm-make-source "TLP" 'tlp--helm-project-class)
        :buffer "*helm TLP*"))

(defun tlp--get-org-heading-info ()
  "Return a cons cell of header text and marker position.
Helm displays the header text and uses the marker position to visit the file."
  (cons (org-get-heading) (point-marker)))

(defun tlp--find-headings ()
  "Return a list of TLP headings in `org-agenda-files'."
  (let ((org-use-tag-inheritance nil))
    (org-map-entries 'tlp--get-org-heading-info
                     tlp--org-tag-match-expression
                     'agenda)))

(defun tlp--helm-action-goto-project (marker)
  "Move to buffer and point of MARKER."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (org-show-entry))

(defclass tlp--helm-project-class (helm-source-sync)
  ((candidates :initform 'tlp--find-headings)
   (action :initform '(("Go to heading" . tlp--helm-action-goto-project))))
  "A class to hold helm information about available TLP projects.")

(defun tlp--extract-org-json-src-block ()
  "Return a string of the org SRC block under the current heading.
If not block is found, display a message and return nil."
  (let ((case-fold-search nil)
        (subtree-end (save-excursion (org-end-of-subtree 'invisible-ok))))
    (save-restriction
      (save-excursion
        (re-search-forward "#\\+BEGIN_SRC json" subtree-end)
        (forward-line)
        (buffer-substring-no-properties
         (point)
         (progn
           (re-search-forward "#\\+END_SRC")
           (line-beginning-position)))))))

(defun tlp--load-config-json (json-string)
  "Parse and return the JSON-STRING.
If JSON is malformed, signal `tlp-config-format'."
  (condition-case nil
      (json-read-from-string json-string)
    (json-error
     (signal 'tlp-config-format '("JSON is malformed in :tlpConfig:")))))

(defun tlp--load-config ()
  "Load the JSON config for the TLP heading at point.
The config is marked with the tag :tlpConfig:.  If no such
heading exists, signal `tlp-missing-config'.  If there are
multiple configs, load the first one."
  (-if-let (org-src-jsons
            (org-map-entries 'tlp--extract-org-json-src-block
                             tlp--org-tag-match-expression-config
                             'tree))
      (tlp--load-config-json (car org-src-jsons))
    (signal 'tlp-missing-config '("No :tlpConfig: JSON SRC block found."))))

(defvar tlp--config-short-name nil
  "The short name of the current project.")

(defvar tlp--config-project-root nil
  "The current absolute path to the project root.")

(defvar tlp--config-repo-branch nil
  "The repository branch for the current project.")

(defvar tlp--config-layouts nil
  "An alist of available layouts.")

(defvar tlp--config-global-marks nil
  "An alist of letters to a position in a file.")

(defvar tlp--config-commands nil
  "Commands for the current project.")

(defvar tlp--config-tmux nil
  "The tmux setup for the current project.")

(defun tlp--reset-config ()
  "Reset config variables."
  (setq tlp--config-short-name nil)
  (setq tlp--config-project-root nil)
  (setq tlp--config-repo-branch nil)
  (setq tlp--config-layouts nil)
  (setq tlp--config-global-marks nil)
  (setq tlp--config-commands nil)
  (setq tlp--config-tmux nil))

(defclass tlp-config-class ()
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of a TLP project.")
   (projectRoot
    :initarg :projectRoot
    :initform ""
    :type string
    :documentation "The absolute path to the root directory.")
   (repoBranch
    :initarg :repoBranch
    :initform ""
    :type string
    :documentation "The branch of the repository to use." )
   (layouts
    :initarg :layouts
    :initform nil
    :documentation "The layouts of the project.")
   (globalMarks
    :initarg :globalMarks
    :initform '()
    :documentation "Global marks for the TLP project.")
   (commands
    :initarg :commands
    :initform '()
    :documentation "Commands for this controlling the TLP project.")
   (tmux
    :initarg :tmux
    :initform ""
    :type string
    :documentation "A Tmuxinator configuration file."))
  "A class to hold the configuration options of a tlp project.")

(defun tlp--set-config-section (tlp-config config-element)
  "Run the correct config initialization based on the car of CONFIG-ELEMENT."
  (pcase config-element
    (`(name . ,name) (oset tlp-config :name name))
    (`(projectRoot . ,file-path) (oset tlp-config :projectRoot
                                       (file-truename file-path)))
    (`(repoBranch . ,branch-name) (oset tlp-config :repoBranch branch-name))
    (`(layouts . ,layout-alist) (oset tlp-config :layouts layout-alist))
    (`(globalMarks . ,marks-alist) (oset tlp-config :globalMarks marks-alist))
    (`(commands . ,cmd-alist) (oset tlp-config :commands cmd-alist))
    (`(tmux . ,tmux-alist) (oset tlp-config :tmux tmux-alist))))

(defun tlp-make-config (config-alist)
  "Initialize a `tlp-config-class' object from CONFIG-ALIST."
  (let ((tlp-config (make-instance 'tlp-config-class)))
    (dolist (config-section config-alist)
      (tlp--set-config-section tlp-config config-section))
    tlp-config))

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
