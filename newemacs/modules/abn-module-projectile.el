;;; abn-module-projectile.el --- Project setup

;;; Commentary:
;;

(use-package projectile

  :general
  (:keymaps 'abn-leader-map
            "pv" 'projectile-vc)
  :init
  (setq projectile-completion-system 'ivy)

  )

(provide 'abn-module-projects)
;;; abn-module-projectile.el ends here
