;;; abn-funcs-javascript.el --- Functions for javascript

;;; Commentary:
;;

;;; Code:

(defun abn/eslint-fix-file ()
  (interactive)
  (message "eslint fixing the file %s" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun abn/js-clang-format-file ()
  (interactive)
  (message "clang formatting file %s" (buffer-file-name))
  (shell-command (concat "clang-format -i -style=Google"
			 (buffer-file-name)))
  (revert-buffer t t))

(defun abn/eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

(defun abn//get-beginning-of-string-point ()
  "Gets the beginning of the string at point.
If point is not in a string or at the beginning of a string, return nil."
  (save-excursion
    (if (looking-at "['\"]")
	;; Assume we're at the beginning of a string so just return.
	(point)
      ;; We're not at the beginning of a string.
      (if (not (nth 3 (syntax-ppss)))
	  ;; We're not inside a string.
	  nil
	;; We're inside a string.
	(while (nth 3 (syntax-ppss))
	  (forward-char -1))
	(point)))))

(defun abn//js-clang-format-string (js-code)
  "Format JS-CODE with clang-format."
  (let ((temp-file (make-temp-file "js-clang-format")))
    (write-region js-code nil temp-file)
    (shell-command (format "clang-format -i %s" temp-file))
    (prog1
	(with-temp-buffer
	  (insert-file-contents temp-file)
	  (buffer-string))
      (delete-file temp-file))))

(defun abn//extract-string-contents ()
  "Gets the contents of the string at point.
If point is not in a string, return nil."
  (save-excursion
    (let ((string-start (abn//get-beginning-of-string-point))
	  string-end)
      (when string-start
	(goto-char string-start)
	(forward-sexp)
	(setq string-end (point))
	(buffer-substring-no-properties (1+ string-start) (1- string-end))))))

(defun abn/javascript-format-js-code-in-string ()
  "Replaces the current string of JS code with nicely formatted code.
Uses template literals to support multiple lines of code."
  (interactive)
  (let ((string-contents (abn//extract-string-contents))
	(string-start (abn//get-beginning-of-string-point)))
    (when string-contents
      (goto-char string-start)
      (kill-sexp)
      (insert (format "`\n%s`" (abn//js-clang-format-string string-contents)))
      )))

(defun abn/javascript-format-file ()
  "Runs clang format on a file."
  (interactive)
  (when (buffer-file-name)
    (shell-command (format "clang-format -i %s" (buffer-file-name)))))

(defun abn//javascript-extend-jsdoc-tags ()
  (setq js2-jsdoc-typed-tag-regexp
	(concat "^\\s-*\\*+\\s-*\\(@\\(?:"
		(regexp-opt
		 '("enum"
		   "extends"
		   "field"
		   "id"
		   "implements"
		   "lends"
		   "mods"
		   "requires"
		   "return"
		   "returns"
		   "throw"
		   "throws"))
		"\\)\\)\\s-*\\({[^}]+}\\)?"))

  (setq js2-jsdoc-arg-tag-regexp
	(concat "^\\s-*\\*+\\s-*\\(@\\(?:"
		(regexp-opt
		 '("alias"
		   "augments"
		   "borrows"
		   "callback"
		   "bug"
		   "base"
		   "config"
		   "default"
		   "define"
		   "exception"
		   "func"
		   "function"
		   "member"
		   "memberOf"
		   "method"
		   "module"
		   "name"
		   "namespace"
		   "since"
		   "suppress"
		   "this"
		   "throws"
		   "type"
		   "version"

		   ;; Added from Annotation.java in jscomp.
		   "idGenerator"
		   "meaning"  ;; Looks like it's for localization.
		   "modifies" ;; Only for externs.
		   "template"
		   "typedef"
		   "visibility" ;; Controls blaze build visibility.
		   ))
		"\\)\\)\\s-+\\([^ \t]+\\)"))

  (setq js2-jsdoc-empty-tag-regexp
	(concat "^\\s-*\\*+\\s-*\\(@\\(?:"
		(regexp-opt
		 '("addon"
		   "author"
		   "class"
		   "const"
		   "constant"
		   "constructor"
		   "constructs"
		   "deprecated"
		   "desc"
		   "description"
		   "event"
		   "example"
		   "exec"
		   "export"
		   "fileoverview"
		   "final"
		   "func"
		   "function"
		   "hidden"
		   "ignore"
		   "implicitCast"
		   "inheritDoc"
		   "inner"
		   "interface"
		   "license"
		   "method"
		   "noalias"
		   "noshadow"
		   "notypecheck"
		   "override"
		   "owner"
		   "preserve"
		   "preserveTry"
		   "private"
		   "protected"
		   "public"
		   "static"
		   "supported"

		   ;; Added from Annotation.java in jscomp.
		   "ngInject"
		   "abstract"
		   "copyright"
		   "disposes"
		   "externs"
		   "record"
		   "jaggerInject"
		   "jaggerModule"
		   "jaggerProvidePromise"
		   "jaggerProvide"
		   "nocollapse"
		   "nosideeffects"
		   "nocompile"
		   "package" ;; Indicates package-private.
		   "polymerBehavior"
		   "struct"
		   "unrestricted" ;; Mark class that's not a @struct or @dict.
		   "wizaction"
		   ))
		"\\)\\)\\s-*")))

(provide 'abn-funcs-javascript)
;;; abn-funcs-javascript.el ends here
