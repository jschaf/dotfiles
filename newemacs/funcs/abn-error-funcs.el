;;; abn-error-funcs.el --- Functions for handling errors

;;; Commentary:
;;

(defun abn//error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun abn/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((delegate (abn//error-delegate)))
    (pcase delegate
      ('flycheck (call-interactively 'flycheck-next-error))
      ('emacs (call-interactively 'next-error))
      (error "Unknown error delegate type."))))

(defun abn/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((delegate (abn//error-delegate)))
    (pcase delegate
      ('flycheck (call-interactively 'flycheck-previous-error))
      ('emacs (call-interactively 'previous-error)))))

(provide 'abn-error-funcs)
;;; abn-error-funcs.el ends here
