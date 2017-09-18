;;; abn-module-emacs-config.el --- Tools for editing the emacs config

;;; Commentary:
;;

(use-package abn-funcs-emacs-config
  :ensure nil ; local package
  :general
  (:keymaps 'abn-leader-map
   "fem" 'abn/new-module))

(provide 'abn-module-emacs-config)
;;; abn-module-emacs-config.el ends here
