;;; abn-module-emacs-config.el --- Tools for editing the emacs config

;;; Commentary:
;;

(use-package abn-funcs-emacs-config
  :ensure nil ; local package
  :defer t
  :commands (abn/new-module-in-dir)
  :bind
  (:map abn-leader-map
   ("fem" . abn/new-module)
   ("td" . toggle-debug-on-error)))

(use-package debug-hooks
  :defer t
  :ensure nil ; development package
  :load-path "~/prog/emacs-debug-hooks"
  :commands (debug-hooks-mode debug-hooks-advise-hooks))

(provide 'abn-module-emacs-config)
;;; abn-module-emacs-config.el ends here
