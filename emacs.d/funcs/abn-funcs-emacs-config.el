;;; abn-funcs-emacs-config.el --- Functions for editing the emacs config

;;; Commentary:
;;

;;; Code:

(require 's)

(defun abn/new-module (name)
  (interactive
   (list (read-string "New module name: ")))
  (abn/new-module-in-dir name abn-dir))

(defun abn/new-module-in-dir (name directory)
  (abn//make-new-module-file name directory)
  (abn//make-new-funcs-file name directory))

(defun abn//make-new-funcs-file (name directory)
  "Creates a new module from NAME."
  (interactive
   (list (read-string "New funcs name: ")))
  (let* ((description (format "Functions for %s" name))
	 (full-file-name (format "abn-funcs-%s.el" name))
	 (funcs-path (concat directory "/funcs/"  full-file-name)))
    (if (file-exists-p funcs-path)
	(error "Funcs `%s' already exists.")
      (find-file funcs-path)
      (insert (abn//new-funcs-file-template name description))
      (goto-char (point-min))
      (search-forward "Code:")
      (forward-line 1)
      (save-buffer))))

(defun abn//new-funcs-file-template (name description)
  (s-join
   "\n"
   (list
    (format ";;; abn-funcs-%s.el --- %s" name description)
    ""
    ";;; Commentary:"
    ";;"
    ""
    ";;; Code:"
    ""
    (format "(provide 'abn-funcs-%s)" name)
    (format ";;; abn-funcs-%s.el ends here" name)
    "" ; trailing newline
    )))

(defun abn//make-new-module-file (name directory)
  "Creates a new module from NAME."
  (interactive
   (list (read-string "New module name: ")))
  (let* ((description (format "Config for %s" name))
	 (full-file-name (format "abn-module-%s.el" name))
	 (module-path (concat directory "/modules/" full-file-name)))
    (if (file-exists-p module-path)
	(error "Module `%s' already exists.")
      (find-file module-path)
      (insert (abn//new-module-file-template name description))
      (goto-char (point-min))
      (search-forward ":ensure nil")
      (save-buffer))))

(defun abn//new-module-file-template (name description)
  "Returns a string of a complete emacs lisp file."
  (s-join
   "\n"
   (list
    (format ";;; abn-module-%s.el --- %s" name description)
    ""
    ";;; Commentary:"
    ";;"
    ""
    ";;; Code:"
    "(eval-when-compile"
    "  (require 'use-package))"
    ""
    (format "(use-package abn-funcs-%s" name)
    "  :ensure nil ; local package"
    "  :general"
    "  (abn/define-leader-keys"
    "   \"fem\" 'abn/new-module)"
    ")"
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
    ""
    "  :config"
    ""
    ")"
    ""
    (format "(provide 'abn-module-%s)" name)
    (format ";;; abn-module-%s.el ends here" name)
    "" ; trailing newline
    )))

(provide 'abn-funcs-emacs-config)
;;; abn-funcs-emacs-config.el ends here
