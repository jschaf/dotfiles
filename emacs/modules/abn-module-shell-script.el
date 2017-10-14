;;; abn-module-shell-script.el --- Config for shell-script

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-shell-script
  :ensure nil ; local package
  :general
  (abn/define-leader-keys
   "zf" #'abn/new-zsh-function
   "zw" #'abn/new-zsh-function-work
   "zi" #'abn/new-zsh-function-iosource
   "zh" #'abn/new-zsh-function-host
   "zk" #'abn/new-zsh-key-widget))

(use-package sh-script
  :defer t
  :ensure nil ; built-in package
  :config
  (setq-default sh-basic-offset 2))

(provide 'abn-module-shell-script)
;;; abn-module-shell-script.el ends here
