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

(use-package go-mode
  :defer t
  :mode
  (("\\.go\\'" . go-mode))
  :init
  (add-hook 'go-mode-hook 'abn/go-mode-hook-init-format-on-save))

(use-package rust-mode
  :defer t
  :mode
  (("\\.rs\\'" . rust-mode))
  :init
  (setq rust-format-on-save t)
  )

(use-package yaml-mode
  :defer t
  :mode
  (("\\.yml\\'" . yaml-mode)))

(provide 'abn-module-langs)
;;; abn-module-langs.el ends here
