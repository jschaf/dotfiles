;;; abn-module-java.el --- Config for java

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-java
  :defer t
  :ensure nil ; local package
  :commands
  (abn/set-column-limit-to-100
   abn/set-java-indent-to-2-spaces))

;; cc-mode defines java-mode
(use-package cc-mode
  :defer t
  :ensure nil ; built-in package
  :init
  :config
  (add-hook 'java-mode-hook 'abn/set-column-limit-to-100)
  (add-hook 'java-mode-hook 'abn/set-java-indent-to-2-spaces))

(provide 'abn-module-java)
;;; abn-module-java.el ends here
