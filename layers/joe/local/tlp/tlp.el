;;; tlp.el -- Project management in Emacs
(defvar tlp--org-tag-match-expression "tlp"
  "The match expression to find project headings.")


(defvar tlp--org-tag-match-expression-config "tlpConfig"
  "The match expression to find project config headings.")

(define-error 'tlp-error "TLP Error.")

;;;###autoload
(defun tlp-start-work ()
  "Prompts for TLP project to open."
  (interactive)
  (tlp-helm-available-projects)
  (tlp--reset-config)
  (-when-let (config (tlp--load-config))
    (tlp--init-config config))
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
If JSON is malformed, display a message and return nil."
  (condition-case nil
      (json-read-from-string json-string)
    (error
     (signal 'tlp-error '("JSON is malformed in :tlpConfig:")))))

(defun tlp--load-config ()
  "Load the JSON config for the TLP heading at point.
The config is marked with the tag :tlpConfig:.  If no such
heading exists, raise an error.  If there are multiple configs,
load the first one."
  (-if-let (org-src-jsons
            (org-map-entries 'tlp--extract-org-json-src-block
                             tlp--org-tag-match-expression-config
                             'tree))
      (tlp--load-config-json (car org-src-jsons))

    (message "No :tlpConfig: JSON SRC block found.")
    nil))

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

(defun tlp--init-config-section (config-element)
  "Run the correct config initialization based on the car of CONFIG-ELEMENT."
  (pcase config-element
    (`(shortName . ,name) (setq tlp--config-short-name) name)
    (`(projectRoot . ,file-path) (setq tlp--config-project-root
                                       (file-truename file-path)))
    (`(repoBranch . ,branch-name) (setq tlp--config-repo-branch branch-name))
    (`(layouts . ,layout-alist) (setq tlp--config-layouts layout-alist))
    (`(globalMarks . marks-alist) (setq tlp--config-global-marks marks-alist))
    (`(commands . cmd-alist) (setq tlp--config-commands cmd-alist))
    (`(tmux . tmux-alist) (setq tlp--config-tmux tmux-alist))))

(defun tlp--init-config (config)
  "Initialize the config."
  (mapcar 'tlp--init-config-section config))

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
