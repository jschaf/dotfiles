;;; abn-funcs-emacs-config.el --- Functions for editing the emacs config

;;; Commentary:
;;

;;; Code:

(require 's)

(defun abn/new-module (name)
  (interactive
   (list (read-string "New module name: ")))
  (abn//make-new-module-file name)
  (abn//make-new-funcs-file name))

(defun abn//make-new-module-file (name)
  "Creates a new module from NAME."
  (interactive
   (list (read-string "New module name: ")))
  (let* ((description (format "Config for %s" name))
	 (full-file-name (format "abn-module-%s.el" name))
	 (module-path (concat abn-modules-dir "/" full-file-name)))
    (if (file-exists-p module-path)
	(error "Module `%s' already exists.")
      (find-file module-path)
      (insert (abn//new-elisp-file-template "module" name description))
      (goto-char (point-min))
      (forward-line 9)
      (insert
       (s-join "\n"
	       (list
                (format "(use-package abn-funcs-%s" name)
                "  :ensure nil ; local package"
                "  :general"
                "  (abn/define-leader-keys"
                "   \"fem\" 'abn/new-module)"
                ")"
                "\n")))

      (save-buffer))))

(defun abn//make-new-funcs-file (name)
  "Creates a new module from NAME."
  (interactive
   (list (read-string "New funcs name: ")))
  (let* ((description (format "Functions for %s" name))
	 (full-file-name (format "abn-funcs-%s.el" name))
	 (funcs-path (concat abn-funcs-dir "/" full-file-name)))
    (if (file-exists-p funcs-path)
	(error "Funcs `%s' already exists.")
      (find-file funcs-path)
      (insert (abn//new-elisp-file-template "funcs" name description))
      (save-buffer))))

(defun abn//new-elisp-file-template (type name description)
  "Returns a string of a complete emacs lisp file."
  (s-join
   "\n"
   (list
    (format ";;; abn-%s-%s.el --- %s" type name description)
    ""
    ";;; Commentary:"
    ";;"
    ""
    ";;; Code:"
    "(eval-when-compile"
    "  (require 'use-package))"
    ""
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
    (format "(provide 'abn-%s-%s)" type name)
    (format ";;; abn-%s-%s.el ends here" type name)
    "" ; trailing newline
    )))

(provide 'abn-funcs-emacs-config)
;;; abn-funcs-emacs-config.el ends here
