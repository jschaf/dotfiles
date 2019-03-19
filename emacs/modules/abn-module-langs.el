;;; abn-module-langs.el --- Config for smallish languages

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-langs
  :ensure nil ; local package
  )

(use-package css-mode
  :defer t
  :ensure nil ; built-in
  :init
  (setq css-indent-offset 2))

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
  (setq rust-format-on-save t))

(use-package scala-mode
  :defer t
  :mode
  (("\\.scala\\'" . scala-mode)))

(use-package sbt-mode
  :defer t
  :mode
  (("\\.sbt\\'" . sbt-mode)))

(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'". typescript-mode)))

(use-package vimrc-mode
  :defer t
  :mode
  (("ideavimrc\\'" . vimrc-mode)))

(use-package yaml-mode
  :defer t
  :mode
  (("\\.yml\\'" . yaml-mode)))

(provide 'abn-module-langs)
;;; abn-module-langs.el ends here
