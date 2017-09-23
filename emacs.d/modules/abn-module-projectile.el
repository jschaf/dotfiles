;;; abn-module-projectile.el --- Project setup

;;; Commentary:
;;

(use-package projectile
  :defer 1
  :diminish projectile-mode
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
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-known-projects-file
        (concat abn-cache-dir "/projectile-bookmarks.eld"))
  (setq projectile-cache-file
        (concat abn-cache-dir "/projectile.cache") )

  (setq projectile-globally-ignored-directories
        '(".bzr"
          ".ensime_cache"
          ".eunit"
          ".fslckout"
          ".git"
          ".hg"
          ".idea"
          ".stack-work"
          ".svn"
          ".tox"
          "READONLY"
          "_FOSSIL_"
          "_darcs"
          "blaze-bin"
          "blaze-genfiles"
          "blaze-google3"
          "blaze-out"
          "blaze-testlogs"
          "node_modules"
          "third_party"
          "vendor"))
  (projectile-mode 1))

(provide 'abn-module-projectile)
;;; abn-module-projectile.el ends here
