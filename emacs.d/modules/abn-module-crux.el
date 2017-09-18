;;; abn-module-crux.el --- Setup for crux.el

;;; Commentary:
;;

(eval-when-compile
  (require 'use-package))
(require 'general)

(use-package crux
  :general
  (:states '(normal visual motion)
   "H" #'crux-move-beginning-of-line))

(provide 'abn-module-crux)
;;; abn-module-crux.el ends here
