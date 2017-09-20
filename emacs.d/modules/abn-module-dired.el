;;; abn-module-dired.el --- Config for dired

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-dired
  :ensure nil ; local package
  :defer t)

(use-package dired-x
  :ensure nil ; built-in package
  :general
  (abn/define-leader-keys
   "fj" 'dired-jump
   "jd" 'dired-jump
   "jD" 'dired-jump-other-window))

(use-package dired
  :ensure nil ; built-in package
  :general
  (abn/define-leader-keys
   "ad" 'dired))

(provide 'abn-module-dired)
;;; abn-module-dired.el ends here
