;;; abn-module-bookmarks.el --- Config for bookmarks

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-bookmarks
  :ensure nil ; local package
  :bind
  (:map abn-leader-map
   ;; Dotfiles
   ("fdc" . abn/bookmark-joe-config)
   ("fdi" . abn/bookmark-i3-conf)
   ("fdm" . abn/bookmark-my-org)
   ("fdp" . abn/bookmark-joe-packages)
   ("fdtm" . abn/bookmark-tmux-conf)
   ("fdx" . abn/bookmark-joe-xinitrc)
   ("fdd" . abn/bookmark-switch-between-func-and-module)

   ;; Else
   ("fdte" . abn/bookmark-else-elisp)
   ("fdts" . abn/bookmark-else-shell)
   ("fdtt" . abn/bookmark-else-template)

   ;; Emacs
   ("fdea" . abn/bookmark-emacs-autocomplete-module)
   ("fdeA" . abn/bookmark-emacs-autocomplete-funcs)
   ("fdeb" . abn/bookmark-emacs-bookmarks-module)
   ("fdeB" . abn/bookmark-emacs-bookmarks-funcs)

   ;; ZSH
   ("fza" . abn/bookmark-zsh-aliases)
   ("fze" . abn/bookmark-zshenv)
   ("fzh" . abn/bookmark-zsh-host)
   ("fzk" . abn/bookmark-zshrc-keys)
   ("fzp" . abn/bookmark-zsh-plugins)
   ("fzr" . abn/bookmark-zsh-profile)
   ("fzz" . abn/bookmark-zshrc)

   ;; Org
   ("fgg" . abn/bookmark-gtd)
   ("fgy" . abn/bookmark-crypto)
   ("fgw" . abn/bookmark-goog)
   ("fgj" . abn/bookmark-journal)
   ("fgs" . abn/bookmark-sandlot)
   ("fgr" . abn/bookmark-refile)
   ("fgt" . abn/bookmark-refile)
   ("fgp" . abn/bookmark-people)
   ("fgl" . abn/bookmark-ledger)
   ("fgc" . abn/bookmark-joe-checklist)
   ("fgR" . abn/bookmark-refile-work)
   ("fgd" . abn/bookmark-org-drill))
  :init
  (abn-declare-prefix "fd" "dotfiles")
  (abn-declare-prefix "fdw" "work")
  (abn-declare-prefix "fz" "zsh")
  (abn-declare-prefix "fg" "org files"))

(provide 'abn-module-bookmarks)
;;; abn-module-bookmarks.el ends here
