;;; abn-module-bbdb.el --- Config for bbdb

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-bbdb
  :ensure nil ; local package
  )

(use-package bbdb
  :ensure t
  :init
  (setq bbdb-variable "~/gdrive/bbdb.el")
  :config)

(provide 'abn-module-bbdb)
;;; abn-module-bbdb.el ends here
