;;; abn-module-coding.el --- Config for general coding.

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-coding
  :ensure nil ; local package
  :commands (abn/pick-zeal-docset))

;; Highlights TODO and similar keywords in comments and strings.
(use-package hl-todo
  :defer t
  :general
  (abn/define-leader-keys
   "et" 'hl-todo-next
   "eT" 'hl-todo-previous)
  :init
  (add-hook 'prog-mode-hook 'hl-todo-mode))

;; Highlights matching parens.  Included in Emacs.
(use-package paren
  :defer 1
  :ensure nil ; built-in package
  :config
  ;; Show paren mode is a global minor mode.  No need for hooks.
  (show-paren-mode 1))

;; Converts between foo_bar, FOO_BAR, and FooBar style of names.
(use-package string-inflection
  :defer t
  :general
  (abn/define-leader-keys
   "xii" 'string-inflection-all-cycle
   "xiu" 'string-inflection-underscore
   "xiU" 'string-inflection-upcase
   "xik" 'string-inflection-kebab-case
   "xic" 'string-inflection-lower-camelcase
   "xiC" 'string-inflection-camelcase))

;; Searches the word at point with Zeal, a documentation browser.
(use-package zeal-at-point
  :defer t
  ;; TODO: configure zeal for easy switching.
  :disabled
  )

(provide 'abn-module-coding)
;;; abn-module-coding.el ends here
