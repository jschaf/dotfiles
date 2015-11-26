;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar joe-packages
  '(
    auto-completion
    diff-hl
    emacs-lisp
    evil
    htmlize
    jinja2-mode
    magit
    org
    python
    rust
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
                     :trigger "jji")))
    :config
    (progn
      (add-hook 'jinja2-mode-hook 'smartparens-mode)))

(defun joe/pre-init-diff-hl ()
  (spacemacs|use-package-add-hook diff-hl
    :post-config
    (progn
      (defun org-font-lock-ensure ()
        (font-lock-fontify-buffer))

      (setq diff-hl-side 'left))))

;; We should be able to call this in use a `use-package', but the order is
;; messed up. See: https://github.com/syl20bnr/spacemacs/issues/2909
(defun joe/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config
    (progn
      (my:evil-keybindings))))

(defun joe/pre-init-magit ()
    (spacemacs|use-package-add-hook magit
      :post-config
      (progn
        (spacemacs|evilify-map magit-status-mode-map
          :mode magit-status-mode
          :bindings
          (kbd "C-j") 'scroll-up-command
          (kbd "C-k") 'scroll-down-command
          (kbd "M-g") 'magit-refresh
          (kbd "C-M-k") 'magit-discard))))

(defun joe/init-htmlize ()
  (use-package htmlize
    :init
    (progn)))

(defun joe/init-typescript-mode ()
  (use-package typescript
    :init
    (progn
      (with-eval-after-load 'compile
        (add-to-list 'compilation-error-regexp-alist 'typescript)
        (add-to-list 'compilation-error-regexp-alist-alist 
                     '(typescript "^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
                                  1 2 3 nil 1))


        (add-to-list 'compilation-error-regexp-alist 'typescript-lint)
        ;; ornament/static/js/main.ts[176, 34]: expected parameter: 'error' to have a typedef
        (add-to-list 'compilation-error-regexp-alist-alist
                     '(typescript-lint "^\\(.+?\\)\\[\\([[:digit:]]+\\), \\([[:digit:]]+\\)\\]: \\(.*\\)$"
                                       1 2 3 nil 1))))))

;; (defun joe/init-color-theme-solarized ()
;;   ;; (use-package color-theme-solarized)
;;   ;; (enable-theme 'solarized)
;;   (load-theme 'solarized t)
;;   )
;; For each package, define a function joe/init-<package-joe>
;;
;; (defun joe/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
