;;; abn-funcs-emacs-config.el --- Functions for editing the emacs config

;;; Commentary:
;;

;;; Code:

(require 's)

(defun abn/new-module (name)
  "Create an abn-funcs-NAME and abn-module-NAME in `abn-dir'"
  (interactive
   (list (read-string "New module name: ")))
  (abn/new-module-in-dir name "" abn-dir))

(defun abn/new-module-in-dir (name abn-suffix directory)
  "Create an abn-funcs-NAME and abn-module-NAME in DIRECTORY"
  (abn//make-new-module-file name abn-suffix directory)
  (abn//make-new-funcs-file name abn-suffix directory)
  (abn//add-module-to-init-in-dir name abn-suffix directory))

(defun abn//add-module-to-init-in-dir (name abn-suffix directory)
  "Adds a require for the NAME module in the DIRECTORY init file."
  (let* ((full-name (concat "abn-" abn-suffix "module-" name))
         (is-work (s-starts-with? "work" abn-suffix))
         (init-file-name (if is-work "work-init.el" "start.el"))
         (init-file-path (concat directory "/" init-file-name))
         (init-buffer (find-file-noselect init-file-path))
         insertion-point
         last-point
         module-name)
    (save-excursion
      (with-current-buffer init-buffer
        (goto-char (point-min))
        ;; Get the point on the line just before where we should insert.
        (setq insertion-point
              (or
               (catch 'break
                 (while (re-search-forward
                         "require 'abn-\\(work-\\)?module-\\([a-z-]+\\)"
                         nil 'noerror)
                   (setq module-name (match-string 2))

                   ;; This module is after ours, so go to prevous line
                   ;; and break.
                   (when (string> module-name name)
                     (forward-line -1)
                     (throw 'break (line-end-position)))

                   ;; We fell off the end of the list.
                   (setq last-point (line-end-position))))
               last-point))

        (goto-char insertion-point)
        (insert (format "\n(require '%s)" full-name))
        (save-buffer)))))

(defun abn//make-new-funcs-file (name abn-suffix directory)
  "Creates a new module from NAME."
  (let* ((full-name (concat "abn-" abn-suffix "funcs-" name))
         (description (format "Functions for %s" name))
	 (funcs-path (concat directory "/funcs/"  full-name ".el")))
    (if (file-exists-p funcs-path)
	(error "Funcs `%s' already exists." funcs-path)
      (find-file funcs-path)
      (insert (abn//new-funcs-file-template full-name description))
      (goto-char (point-min))
      (search-forward "Code:")
      (forward-line 1)
      (save-buffer))))

(defun abn//new-funcs-file-template (full-name description)
  (s-join
   "\n"
   (list
    (format ";;; %s.el --- %s" full-name description)
    ""
    ";;; Commentary:"
    ";;"
    ""
    ";;; Code:"
    ""
    (format "(provide '%s)" full-name)
    (format ";;; %s.el ends here" full-name)
    "" ; trailing newline
    )))

(defun abn//make-new-module-file (name abn-suffix directory)
  "Creates a new module from NAME."
  (let* ((description (format "Config for %s" name))
         (full-name (concat "abn-" abn-suffix "module-" name))
	 (module-path (concat directory "/modules/" full-name ".el")))
    (if (file-exists-p module-path)
	(error "Module `%s' already exists." module-path)
      (find-file module-path)
      (insert (abn//new-module-file-template full-name description))
      (goto-char (point-min))
      (search-forward ":ensure nil")
      (save-buffer))))

(defun abn//new-module-file-template (full-name description)
  "Returns a string of a complete emacs lisp file."
  (s-join
   "\n"
   (list
    (format ";;; %s.el --- %s" full-name description)
    ""
    ";;; Commentary:"
    ";;"
    ""
    ";;; Code:"
    "(eval-when-compile"
    "  (require 'use-package))"
    ""
    (format "(use-package %s"
            (s-replace "module" "funcs" full-name))
    "  :ensure nil ; local package"
    "   )"
    "\n\n"
    (format "(provide '%s)" full-name)
    (format ";;; %s.el ends here" full-name)
    "" ; trailing newline
    )))

(provide 'abn-funcs-emacs-config)
;;; abn-funcs-emacs-config.el ends here
