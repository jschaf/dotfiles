;;; abn-module-python.el --- Config for python

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-python
  :ensure nil ; local package
  )


(use-package python
  :defer t
  :ensure nil ;built-in package
  :config
  ;; Prevent eldoc errors.
  (add-hook 'python-mode-hook
            (lambda ()
              (kill-local-variable 'eldoc-documentation-function))))

(provide 'abn-module-python)
;;; abn-module-python.el ends here
