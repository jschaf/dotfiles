;;; abn-smartparens-funcs.el --- Functions for smartparens


;;; Commentary:
;;

;;; Code:

(defun abn//conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode)))

(defun abn/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun abn/smartparens-pair-newline-and-indent (id action context)
  (abn/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(provide 'abn-smartparens-funcs)
;;; abn-smartparens-funcs.el ends here
