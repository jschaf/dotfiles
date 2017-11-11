;;; abn-module-javascript.el --- Config for javascript

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-javascript
  :ensure nil ; local package
  :commands (abn//javascript-extend-jsdoc-tags))

(use-package js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (abn/define-leader-keys-for-major-mode 'js2-mode
    "fs" 'abn/javascript-format-js-code-in-string
    "ff" 'abn/javascript-format-file
    "rf" 'abn/eslint-fix-file-and-revert
    "za" 'js2-mode-toggle-element
    "ze" 'js2-mode-toggle-element)
  :config
  (setq-default js2-basic-offset 2)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq-default js2-mode-show-strict-warnings nil)
  (abn//javascript-extend-jsdoc-tags)

  (defun abn/shorten-js2-mode-name ()
    (setq mode-name "JS"))
  (add-hook 'js2-mode-hook #'abn/shorten-js2-mode-name))

(provide 'abn-module-javascript)
;;; abn-module-javascript.el ends here
