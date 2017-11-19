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

(defun abn/else-kill-always ()
  "Like `else-kill' but kills required placeholders without prompting."
  (interactive)
  (else-kill 'force))


(defvar abn--else-next-placeholder-threshold 5
  "Goto the next placeholder after a kill if less than n lines away.")

(defun abn/else-next-if-nearby ()
  "Proceed to next placeholder if near enough to the current point."
  (interactive)
  (let* ((here (current-point))
         (here-line (line-number-at-pos here))
         (next-point (progn (else-next 1 :no-error-msg t)
                            (point)))
         (next-line (line-number-at-pos next-point)))
    ;; The point was moved in `else-next'.  Only go back if we exceed
    ;; the threshold.
    (when (> (- next-line here-line) abn--else-next-placeholder-threshold)
      (goto-char here))))

(provide 'abn-funcs-autocomplete)
;;; abn-funcs-autocomplete.el ends here
