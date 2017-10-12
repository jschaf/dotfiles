;;; abn-module-magit.el --- Config for magit

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-magit
  :ensure nil ; local package
  )

(use-package magit
  :defer t
  :general
  (:keymaps 'magit-mode-map
   "SPC" abn-leader-map))

(provide 'abn-module-magit)
;;; abn-module-magit.el ends here
