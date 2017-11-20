;;; abn-module-tty.el --- Config for tty

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-tty
  :ensure nil ; local package
  )

;; The inactive background is indistinguishable from the normal
;; background.
(set-face-background 'mode-line "brightmagenta")
(set-face-background 'mode-line-inactive "black")

(provide 'abn-module-tty)
;;; abn-module-tty.el ends here
