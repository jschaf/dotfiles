;;; abn-module-highlight.el --- Config for highlight

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-highlight
  :ensure nil ; local package
  :commands (abn/highlight-symbol))

(use-package auto-highlight-symbol
  :defer t
  :bind
  (:map abn-leader-map
   ("sh" . abn/highlight-symbol)))

(provide 'abn-module-highlight)
;;; abn-module-highlight.el ends here
