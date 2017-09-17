;;; abn-crux.el --- Setup for crux.el

;;; Commentary:
;;

(use-package crux
  :ensure t
  :general
  (general-define-key :states '(normal visual motion)
                      "H" #'crux-move-beginning-of-line))

(provide 'abn-crux)
;;; abn-crux.el ends here
