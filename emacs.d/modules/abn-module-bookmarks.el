;;; abn-module-bookmarks.el --- Config for bookmarks

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-bookmarks
  :ensure nil ; local package
  :general
  (:keymaps 'abn-leader-map
   ;; Dotfiles
   "fdc" 'abn/bookmark-joe-config
   "fdi" 'abn/bookmark-i3-conf
   "fdm" 'abn/bookmark-my-org
   "fdp" 'abn/bookmark-joe-packages
   "fdt" 'abn/bookmark-tmux-conf
   "fdT" 'abn/bookmark-joe-tlp
   "fdx" 'abn/bookmark-joe-xinitrc

   ;; ZSH
   "fdzz" 'abn/bookmark-zshrc
   "fdzk" 'abn/bookmark-zshrc-keys
   "fdzp" 'abn/bookmark-zsh-plugins
   "fdze" 'abn/bookmark-zshenv
   "fdzr" 'abn/bookmark-zsh-prompts
   "fdzh" 'abn/bookmark-zsh-host
   "fdzw" 'abn/bookmark-zsh-work
   "fdhr" 'abn/bookmark-hgrc

   ;; Org
   "fgg" 'abn/bookmark-gtd
   "fgw" 'abn/bookmark-goog
   "fgj" 'abn/bookmark-journal
   "fgs" 'abn/bookmark-sandlot
   "fgr" 'abn/bookmark-refile
   "fgt" 'abn/bookmark-refile
   "fgp" 'abn/bookmark-people
   "fgl" 'abn/bookmark-ledger
   "fgc" 'abn/bookmark-joe-checklist
   "fgR" 'abn/bookmark-refile-work
   "fgd" 'abn/bookmark-org-drill)
  :init
  (abn-declare-prefix "fd" "dotfiles")
  (abn-declare-prefix "fdz" "zsh")
  (abn-declare-prefix "fg" "org files"))

(provide 'abn-module-bookmarks)
;;; abn-module-bookmarks.el ends here
