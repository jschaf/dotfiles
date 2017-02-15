;;; tlp.el -- Project management in Emacs
(defvar tlp--org-tag-match-expression "tlp"
  "The match expression to find project headings.")


(defvar tlp--org-tag-match-expression-config "tlpConfig"
  "The match expression to find project config headings.")


(defvar tlp-current-config nil
  "The config for the current TLP project.")

(define-error 'tlp-error "TLP Error")
(define-error 'tlp-config-format "Bad JSON config" 'tlp-error)
(define-error 'tlp-missing-config "Can't find config" 'tlp-error)

;;;###autoload
(defun tlp-start-work ()
  "Prompts for TLP project to open."
  (interactive)
  (tlp-helm-available-projects)
  (org-narrow-to-subtree)
  (tlp-init-project))

(defun tlp-init-project ()
  "Initialize the project using CONFIG."
  (-when-let (config (tlp-load-config))
    (tlp-config-init-repo-branch config)
    (tlp-config-init-tmux-session config)))

(defun tlp-load-config ()
  "Loads and returns the `tlp-config-class' for the TLP project at point."
  (condition-case nil
      (let ((config-alist (tlp--parse-json-config)))
        (tlp-make-config config-alist))
    (tlp-config-format
     (message "Can't read JSON config.")
     nil)
    (tlp-missing-config
     (message "No JSON config found.")
     nil)))

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
If no block is found, display a message and return nil."
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

(defun tlp--parse-json (json-string)
  "Parse and return the JSON-STRING.
If JSON is malformed, signal `tlp-config-format'."
  (condition-case nil
      (json-read-from-string json-string)
    (json-error
     (signal 'tlp-config-format '("JSON is malformed in :tlpConfig:")))))

(defun tlp--parse-json-config ()
  "Parse the JSON config for the TLP heading at point.
The config is marked with the tag :tlpConfig:.  If no such
heading exists, signal `tlp-missing-config'.  If there are
multiple configs, load the first one."
  (-if-let (org-src-jsons
            (org-map-entries 'tlp--extract-org-json-src-block
                             tlp--org-tag-match-expression-config
                             'tree))
      (tlp--parse-json (car org-src-jsons))
    (signal 'tlp-missing-config '("No :tlpConfig: JSON SRC block found."))))

(defclass tlp-config-class ()
  ((name
    :initarg :name
    :initform ""
    :type string
    :documentation "The name of a TLP project.")
   (project-root
    :initarg :project-root
    :initform ""
    :type string
    :documentation "The absolute path to the root directory.")
   (repo-branch
    :initarg :repo-branch
    :initform ""
    :type string
    :documentation "The branch of the repository to use." )
   (layouts
    :initarg :layouts
    :initform nil
    :documentation "The layouts of the project.")
   (global-marks
    :initarg :global-marks
    :initform '()
    :documentation "Global marks for the TLP project.")
   (commands
    :initarg :commands
    :initform '()
    :documentation "Commands for this controlling the TLP project.")
   (tmux-session
    :initarg :tmux-session
    :initform ""
    :type string
    :documentation "A Tmuxinator configuration file."))
  "A class to hold the configuration options of a tlp project.")

(defmethod tlp-config-init-global-marks ((config tlp-config-class))
  "Initialize the global marks for the CONFIG."
  (let (marks (oref config :global-marks))
    (cl-loop for (letter . location) in marks
             )))


(defun tlp--set-config-section (tlp-config config-element)
  "Run the correct config initialization based on the car of CONFIG-ELEMENT."
  (pcase config-element
    (`(name . ,name) (oset tlp-config :name name))
    (`(projectRoot . ,file-path) (oset tlp-config :project-root
                                       (file-truename file-path)))
    (`(repoBranch . ,branch-name) (oset tlp-config :repo-branch branch-name))
    (`(layouts . ,layout-alist) (oset tlp-config :layouts layout-alist))
    (`(globalMarks . ,marks-alist) (oset tlp-config :global-marks marks-alist))
    (`(commands . ,cmd-alist) (oset tlp-config :commands cmd-alist))
    (`(tmuxSession . ,tmux-alist) (oset tlp-config :tmux-session tmux-alist))))

(defun tlp-make-config (config-alist)
  "Initialize a `tlp-config-class' object from CONFIG-ALIST."
  (let ((tlp-config (make-instance 'tlp-config-class)))
    (dolist (config-section config-alist)
      (tlp--set-config-section tlp-config config-section))
    tlp-config))

(defmethod tlp-config-init-tmux-session ((config tlp-config-class))
  "Switch the tmux session to the one specified in CONFIG."
  (let ((session (oref config :tmux-session)))
    (when (not (s-blank? session))
      (shell-command
       (format "tmux send-keys 'tmuxp load -y %s' ENTER" session)))))

(defmethod tlp-config-init-repo-branch ((config tlp-config-class))
  "Switch the tmux session to the one specified in CONFIG."
  (let ((project-root (oref config :project-root))
        (repo-branch (oref config :repo-branch)))
    (when (not (s-blank? repo-branch))
      (shell-command
       (format "my-switch-branch.sh %s %s" project-root repo-branch)))))

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
