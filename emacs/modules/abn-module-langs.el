;;; abn-module-langs.el --- Config for smallish languages

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-langs
  :ensure nil ; local package
  )


(use-package vimrc-mode
  :defer t
  :mode
  (("ideavimrc\\'" . vimrc-mode)))

(provide 'abn-module-langs)
;;; abn-module-langs.el ends here
