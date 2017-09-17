;;; abn-crux.el --- Setup for crux.el

;;; Commentary:
;;

;; (eval-when-compile
;;   (require 'use-package))
;; (require 'general)

(use-package crux
  :ensure t
  :general
  (:states '(normal visual motion)
           "H" #'crux-move-beginning-of-line))

(provide 'abn-crux)
;;; abn-crux.el ends here
