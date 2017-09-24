;;; abn-module-highlight.el --- Config for highlight

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-highlight
  :ensure nil ; local package
  :commands (abn/highlight-symbol)
  :general)

(use-package auto-highlight-symbol
  :defer t
  :general
  (abn/define-leader-keys
   "sh" 'abn/highlight-symbol))

(provide 'abn-module-highlight)
;;; abn-module-highlight.el ends here
