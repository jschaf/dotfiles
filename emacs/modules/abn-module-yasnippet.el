;;; abn-module-yasnippet.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-yasnippet
  :ensure nil ; local package
  :general
  (:states '(emacs insert)
   "M-/" 'abn/load-yas-then-yas-expand))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands (yas-maybe-expand)
  ;; :general
  ;; (:states '(insert emacs)
  ;;  "SPC" 'yas-maybe-expand)
  :init
  (setq yas-snippet-dirs
        '("~/.dotfiles/emacs/snippets"))
  (setq yas-verbosity 4)
  :config
  (setq yas-wrap-around-region t)
  )


(provide 'abn-module-yasnippet)
;;; abn-module-yasnippet.el ends here
