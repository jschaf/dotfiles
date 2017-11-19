;;; abn-funcs-autocomplete.el --- Config for autocomplete

;;; Commentary:
;;

;;; Code:

(defun abn/disable-eldoc-mode ()
  "Disable `eldoc-mode' in the current buffer."
  (eldoc-mode -1))

(defvar abn--else-mappings
  '(("ELSE-Template" . "Template")
    ("Elisp" . "Emacs-Lisp")
    ;; ("Shell-script[zsh]" . "Shell")
    ("sh" . "Shell")
    )
  "Mapping from a major mode to an ELSE template language.")

(defun abn//setup-else-mappings ()
  "Set up mapping from major mode names to ELSE template names."
  (cl-loop for (new-name . old-name) in abn--else-mappings
           do
           (unless (assoc new-name else-Alternate-Mode-Names)
             (add-to-list 'else-Alternate-Mode-Names (cons new-name old-name))))
  abn--else-mappings)

(provide 'abn-funcs-autocomplete)
;;; abn-funcs-autocomplete.el ends here
