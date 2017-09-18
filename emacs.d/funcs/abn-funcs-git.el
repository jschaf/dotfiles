;;; abn-funcs-git.el --- Functions for git

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package FOO
  :ensure nil ; local package
  :diminish foo-mode
  :general
  (:states '(normal)
   "fe" 'do-thing
   :keymaps 'foo-mode-map
   "fe" 'do-thing)
  :init

  :config

)

(provide 'abn-funcs-git)
;;; abn-funcs-git.el ends here
