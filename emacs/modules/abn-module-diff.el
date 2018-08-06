;;; abn-module-diff.el --- Config for diff

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-diff
  :ensure nil ; local package
  :commands
  (abn/ediff-copy-both-to-C
   abn/add-copy-both-to-ediff-mode-map))

(use-package ediff
  :ensure nil ; built-in package
  :init
  (add-hook 'ediff-keymap-setup-hook 'abn/add-copy-both-to-ediff-mode-map))

(provide 'abn-module-diff)
;;; abn-module-diff.el ends here
