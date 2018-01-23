;;; abn-module-python.el --- Config for python

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-python
  :ensure nil ; local package
  :commands(abn/remove-colon-from-electric-indent-chars))


(use-package python
  :defer t
  :ensure nil ;built-in package
  :init
  (setq python-indent-offset 2)
  :config
  ;; Prevent eldoc errors.
  (add-hook 'python-mode-hook
            (lambda ()
              (kill-local-variable 'eldoc-documentation-function)))
  (add-hook 'python-mode-hook
            #'abn/inihibit-electric-indent-mode))

(provide 'abn-module-python)
;;; abn-module-python.el ends here
