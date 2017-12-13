;;; abn-module-help.el --- Config for help

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-help
  :ensure nil ; local package
  :commands (abn/describe-keymap abn/describe-system-info)
  :bind
  (:map abn-leader-map
   ("hdK" . abn/describe-keymap)))

(use-package cust-edit
  :defer t
  :ensure nil ; built-in package
  :init
  (define-key custom-mode-map (kbd "SPC") abn-leader-map))

(provide 'abn-module-help)
;;; abn-module-help.el ends here
