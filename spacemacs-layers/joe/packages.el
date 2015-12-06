;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar joe-packages
  '(
    auto-completion
    diff-hl
    ebib
    emacs-lisp
    evil
    helm-bibtex
    help-fns+
    htmlize
    jinja2-mode
    key-chord
    magit
    org
    persistent-scratch
    request
    s
    typescript
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/init-ebib ()
  (use-package ebib
    :init (progn)))

(defun joe/init-helm-bibtex ()
  (use-package helm-bibtex
    :init (progn)))

(defun joe/init-key-chord ()
  (use-package key-chord
    :init (progn)))

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

(defun joe/init-htmlize ()
  (use-package htmlize
    :init
    (progn)))

(defun joe/init-persistent-scratch ()
  (use-package persistent-scratch
    :init
    (progn
      (persistent-scratch-autosave-mode 1)
      ;; Don't clog up .emacs.d
      (setq persistent-scratch-save-file "~/.emacs-persistent-scratch")

      ;; Ensure file exists
      (unless (file-exists-p persistent-scratch-save-file)
        (write-region "" nil persistent-scratch-save-file))

      (with-current-buffer "*scratch*"
        (lisp-interaction-mode)
        (if (= (buffer-size) 0)
            (persistent-scratch-restore)

          (save-excursion
            (goto-char (point-max))
            (insert "\n\n;; Old Scratch\n\n"))
          (with-temp-buffer
            (insert-file-contents persistent-scratch-save-file)
            (append-to-buffer "*scratch*" (point-min) (point-max)))))

      (defun joe--advise-write-file-for-scratch (orig-fun &rest args)
        (if (eq (current-buffer) (get-buffer "*scratch*"))
            (progn (persistent-scratch-save)
                   (message "Wrote *scratch* to %s." persistent-scratch-save-file))
          (apply orig-fun args)))

      (advice-add 'spacemacs/write-file :around
                  #'joe--advise-write-file-for-scratch))))

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

;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
