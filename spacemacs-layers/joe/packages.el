;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar joe-packages
  '(
    auto-completion
    emacs-lisp
    evil
    jinja2-mode
    python
    typescript
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/init-jinja2-mode ()
  (use-package jinja2
    :defer t
    :init
    (progn
      (defun my-jinja2-block (id action context)
        (insert " ")
        (save-excursion
          (insert " ")))

      (require 'smartparens)
      (add-to-list 'sp-navigate-consider-stringlike-sexp
                   'jinja2-mode)

      ;; Remove curly brace binding because it prevents
      ;; a binding for Jinja constructs.
      (sp-local-pair 'jinja2-mode "{" "}" :actions nil)
      (sp-local-pair 'jinja2-mode "{%" "%}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jjb")
      (sp-local-pair 'jinja2-mode "{{" "}}"
                     :post-handlers '(:add my-jinja2-block)
                     :trigger "jji"))
      )
    :config
    (progn
      (add-hook 'jinja2-mode-hook 'smartparens-mode)
    )
  )

;; For each package, define a function joe/init-<package-joe>
;;
;; (defun joe/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
