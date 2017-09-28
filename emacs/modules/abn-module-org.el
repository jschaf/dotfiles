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
  (abn/define-leader-keys
   ",dd" 'abn/org-set-tag-as-drill
   ",dt" 'abn/org-drill-create-template
   ",dc" 'abn/org-drill-create-template-cloze)
  :init
  (abn-declare-prefix ",d" "org drill"))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :config
  ;; Don't indent under headers.
  (setq org-adapt-indentation nil))

(use-package org-drill
  :defer t
  :ensure org-plus-contrib
  :commands (org-drill)
  :config
  ;; Config options
  )

(use-package org-babel
  :defer t
  :disabled ;; TODO: lazy load me
  :ensure nil
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sh . t)
     (shell . t)
     (haskell . t)
     (js . t)
     (latex . t)
     (gnuplot . t)
     (C . t)
     (sql . t)
     (ditaa . t)))
  :config
  ;; Don't ask to eval code in SRC blocks.
  (setq org-confirm-babel-evaluate nil))

(provide 'abn-module-org)
;;; abn-module-org.el ends here
