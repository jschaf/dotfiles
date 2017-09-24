;;; abn-funcs-yasnippet.el --- Functions for yasnippet

;;; Commentary:
;;

;;; Code:

(defun abn/load-yas-then-yas-expand ()
  (interactive)
  (yas-global-mode 1)
  (general-define-key
   :states '(emacs insert)
    "M-/" 'yas-expand-from-trigger-key)
  (yas-expand-from-trigger-key))

(provide 'abn-funcs-yasnippet)
;;; abn-funcs-yasnippet.el ends here
