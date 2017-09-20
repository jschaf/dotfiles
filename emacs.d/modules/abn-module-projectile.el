;;; abn-module-projectile.el --- Project setup

;;; Commentary:
;;

(use-package projectile
  :commands
  (projectile-ack
   projectile-ag
   projectile-compile-project
   projectile-dired
   projectile-find-dir
   projectile-find-file
   projectile-find-tag
   projectile-test-project
   projectile-grep
   projectile-invalidate-cache
   projectile-kill-buffers
   projectile-multi-occur
   projectile-project-p
   projectile-project-root
   projectile-recentf
   projectile-regenerate-tags
   projectile-replace
   projectile-replace-regexp
   projectile-run-async-shell-command-in-root
   projectile-run-shell-command-in-root
   projectile-switch-project
   projectile-switch-to-buffer
   projectile-vc)

  :general
  (abn/define-leader-keys
   "p!" 'projectile-run-shell-command-in-root
   "p%" 'projectile-replace-regexp
   "p&" 'projectile-run-async-shell-command-in-root
   "pD" 'projectile-dired
   "pF" 'projectile-find-file-dwim
   "pG" 'projectile-regenerate-tags
   "pI" 'projectile-invalidate-cache
   "pR" 'projectile-replace
   "pT" 'projectile-test-project
   "pa" 'projectile-toggle-between-implementation-and-test
   "pb" 'projectile-switch-to-buffer
   "pc" 'projectile-compile-project
   "pd" 'projectile-find-dir
   "pf" 'projectile-find-file
   "pg" 'projectile-find-tag
   "ph" 'helm-projectile
   "pk" 'projectile-kill-buffers
   "pp" 'projectile-switch-project
   "pr" 'projectile-recentf
   "pv" 'projectile-vc
   "pv" 'projectile-vc)
  :init
  (setq projectile-completion-system 'ivy))

(provide 'abn-module-projectile)
;;; abn-module-projectile.el ends here
