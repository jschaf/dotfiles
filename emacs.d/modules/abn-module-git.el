;;; abn-module-git.el --- Config for git

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-git
  :ensure nil ; local package
  :general
  (:keymaps 'abn-leader-map
   "fem" 'abn/new-module)
  )

(use-package magit
  :general
  (:keymaps 'abn-leader-map
   "gfh" 'magit-log-buffer-file
   "gm"  'magit-dispatch-popup
   "gs"  'magit-status
   "gS"  'magit-stage-file
   "gU"  'magit-unstage-file)

  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  )

(provide 'abn-module-git)
;;; abn-module-git.el ends here
