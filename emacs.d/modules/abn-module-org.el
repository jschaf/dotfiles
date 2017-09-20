;;; abn-module-org.el --- Config for org

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-org
  :defer t
  :ensure nil ; local package
  :general
  (:keymaps 'abn-leader-map
   ",dd" 'abn/org-set-tag-as-drill
   ",dt" 'abn/org-drill-create-template
   ",dc" 'abn/org-drill-create-template-cloze)
  :init
  (abn-declare-prefix ",d" "org drill"))

(provide 'abn-module-org)
;;; abn-module-org.el ends here
