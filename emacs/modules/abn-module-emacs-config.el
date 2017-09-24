;;; abn-module-emacs-config.el --- Tools for editing the emacs config

;;; Commentary:
;;

(use-package abn-funcs-emacs-config
  :ensure nil ; local package
  :defer t
  :commands (abn/new-module-in-dir)
  :general
  (abn/define-leader-keys
   "fem" 'abn/new-module
   "td" 'toggle-debug-on-error))

(provide 'abn-module-emacs-config)
;;; abn-module-emacs-config.el ends here
