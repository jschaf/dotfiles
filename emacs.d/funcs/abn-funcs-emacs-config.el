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
  (abn//make-new-module-file
   name
   abn-suffix
   directory)

  (abn//make-new-funcs-file
   name
   abn-suffix
   directory))

(defun abn//make-new-funcs-file (name abn-suffix directory)
  "Creates a new module from NAME."
  (let* ((full-name (concat "abn-" abn-suffix "funcs-" name))
         (description (format "Functions for %s" name))
	 (funcs-path (concat directory "/funcs/"  full-name ".el")))
    (if (file-exists-p funcs-path)
	(error "Funcs `%s' already exists.")
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
	(error "Module `%s' already exists.")
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
    "\n"
    "(use-package FOO"
    "  :defer t"
    "  :diminish foo-mode"
    "  :general"
    "  (abn/define-leader-keys"
    "   \"fe\" 'do-thing"
    "   :keymaps 'foo-mode-map"
    "   \"fe\" 'do-thing)"
    "  :init"
    "  :config"
    ")"
    ""
    (format "(provide '%s)" full-name)
    (format ";;; %s.el ends here" full-name)
    "" ; trailing newline
    )))

(provide 'abn-funcs-emacs-config)
;;; abn-funcs-emacs-config.el ends here
