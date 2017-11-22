;;; abn-module-help.el --- Config for help

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-help
  :ensure nil ; local package
  :commands (abn/describe-keymap abn/describe-system-info)
  :general
  (abn/define-leader-keys
   "hdK" 'abn/describe-keymap))

(use-package cust-edit
  :defer t
  :ensure nil ; built-in package
  :general
  (:keymaps 'custom-mode-map
   "SPC" abn-leader-map))

(provide 'abn-module-help)
;;; abn-module-help.el ends here
