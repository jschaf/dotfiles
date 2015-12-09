;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org

(defvar joe-packages
  '(
    auto-completion
    ebib
    emacs-lisp
    evil
    evil-escape
    helm-bibtex
    help-fns+
    jinja2-mode
    key-chord
    org
    (org-ref
     :location local)
    (otb
     :location local)
    persistent-scratch
    pos-tip
    request
    s
    typescript
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/init-ebib ()
  (use-package ebib))

(defun joe/init-helm-bibtex ()
  (use-package helm-bibtex))

(defun joe/init-key-chord ()
  (use-package key-chord))

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

(defun joe/post-init-evil ()
  (use-package evil
    :config
    (progn

      (defmacro my:make-evil-line-move-motion (name multiplier)
        `(evil-define-motion ,name (count)
           ,(format "Move the cursor (COUNT * %s) lines down." multiplier)
           :type line
           (let (line-move-visual)
             (evil-next-visual-line (* ,multiplier (or count 1))))))

      (my:make-evil-line-move-motion my:evil-next-visual-line-5 5)
      (my:make-evil-line-move-motion my:evil-previous-visual-line-5 -5)


      (define-key evil-normal-state-map "\M-k" 'spacemacs/evil-smart-doc-lookup)
      (define-key evil-normal-state-map "K" 'my:evil-previous-visual-line-5)
      (cl-loop for (key . func) in
               `(("J" . my:evil-next-visual-line-5)
                 ("K" . my:evil-previous-visual-line-5)
                 ("gj" . evil-join)
                 ("H" . my:back-to-indentation-or-beginning)
                 ("L" . evil-end-of-line)
                 ("\C-j" . scroll-up-command)
                 ("\C-k" . scroll-down-command))
               do
               (define-key evil-normal-state-map key func)
               (define-key evil-visual-state-map key func)
               (define-key evil-motion-state-map key func))
      ;; Make movement keys work on visual lines instead of acutal lines.
      ;; This imitates Emacs behavior rather than Vim behavior.
      (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
        'evil-next-visual-line)
      (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
        'evil-previous-visual-line)

      ;; We need to add text before we can edit it.
      (add-to-list 'evil-insert-state-modes 'git-commit-mode)

      (unless window-system
        ;; C-i is the same as tab in the terminal
        (setq evil-want-C-i-jump nil)
        ;; I'm not sure why the above variable isn't respected. I think it's evil's
        ;; fault. I didn't see any key rebinding in spacemacs.
        (define-key evil-motion-state-map "\C-i" nil)))))

(defun joe/post-init-evil-escape ()
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/post-init-org ()
  (use-package org
    :config
    (progn
      (setq org-src-fontify-natively t)

      (defun my:make-org-link-cite-key-visible (&rest _)
        "Make the org-ref cite link visible in descriptive links."
        (when (string-prefix-p "cite:" (match-string 1))
          (remove-text-properties (+ (length "cite:") (match-beginning 1))
                                  (match-end 1)
                                  '(invisible))))

      ;; (advice-add 'org-activate-bracket-links :after #'my:make-org-link-cite-key-visible)
      ;; (advice-remove 'org-activate-bracket-links #'my:make-org-link-cite-key-visible)
      )))

(defun joe/init-persistent-scratch ()
  (use-package persistent-scratch
    :config
    (progn
      (persistent-scratch-autosave-mode 1)
      ;; Don't clog up .emacs.d
      (setq persistent-scratch-save-file "~/.emacs-persistent-scratch")

      ;; Ensure file exists
      (unless (file-exists-p persistent-scratch-save-file)
        (write-region "" nil persistent-scratch-save-file))

      (with-current-buffer "*scratch*"
        (emacs-lisp-mode)
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

(defun joe/init-pos-tip ()
  "Init pos-tip."
  (use-package pos-tip
    :config
    (progn
      (defun describe-thing-in-popup ()
        (interactive)
        (let* ((thing (symbol-at-point))
               (help-xref-following t)
               (description (save-excursion
                              (with-temp-buffer
                                (help-mode)
                                (describe-symbol thing)
                                (buffer-string)))))
          (pos-tip-show description))))))

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

(defun joe/init-otb ()
  (use-package otb
    :config
    (progn
      (joe/set-leader-keys
       "tm" 'my:toggle-mac-modifiers
       "bb" 'my:switch-to-blah-buffer
       "bB" 'my:new-blah-buffer
       "cb" 'joe-blog-compile
       "cB" '(lambda () (interactive) (joe-blog-compile 'force))
       "cp" 'joe-blog-publish
       "cP" 'joe-blog-purge-everything))
    ))



(defun joe/init-org-ref ()
  (use-package org-ref
    :config
    (progn
      ;; optional but very useful libraries in org-ref
      ;; (require 'doi-utils)
      ;; (require 'jmax-bibtex)
      ;; (require 'pubmed)
      ;; (require 'arxiv)
      ;; (require 'sci-id)
      (require 'bibtex)
      (require 'reftex-cite)

      ;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")

      ;;       org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      ;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      ;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"

      ;;       helm-bibtex-bibliography "~/Dropbox/bibliography/references.bib"
      ;;       helm-bibtex-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      ;;       helm-bibtex-notes-path "~/Dropbox/bibliography/helm-bibtex-notes"

      ;;       bibtex-file-path ".:~/Dropbox/bibliography/"
      ;;       )
      )))
