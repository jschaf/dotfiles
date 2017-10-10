;;; abn-module-shell-script.el --- Config for shell-script

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-shell-script
  :ensure nil ; local package
  )


(use-package sh-script
  :defer t
  :ensure nil ; built-in package
  :config
  (setq-default sh-basic-offset 2))

(provide 'abn-module-shell-script)
;;; abn-module-shell-script.el ends here
