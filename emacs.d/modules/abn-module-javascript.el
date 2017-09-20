;;; abn-module-javascript.el --- Config for javascript

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-javascript
  :ensure nil ; local package
  :commands (abn//javascript-extend-jsdoc-tags)
  :general
  (:keymaps 'abn-leader-map
   "fem" 'abn/new-module)
  )

(use-package js2-mode
  :defer t
  :diminish '("Javascript-IDE" . "JS")
  :general
  (:keymaps 'js2-mode-map
   "fs" 'abn/javascript-format-js-code-in-string
   "ff" 'abn/javascript-format-file
   "rf" 'abn/eslint-fix-file-and-revert
   "za" 'js2-mode-toggle-element
   "ze" 'js2-mode-toggle-element)
  :init

  :config
  (setq-default js2-basic-offset 2)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq-default js2-mode-show-strict-warnings nil)
  (abn//javascript-extend-jsdoc-tags))

(provide 'abn-module-javascript)
;;; abn-module-javascript.el ends here
