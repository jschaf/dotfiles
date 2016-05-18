;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org


;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'use-package))

(defvar joe-packages
  '(
    ;; auto-yasnippet
    ;; (doc-popup
    ;; :location local)
    ;; ebib
    camcorder
    ;; emacs-lisp
    evil
    evil-escape
    framemove
    ;; gradle-mode
    ;; helm-bibtex
    ;; help-fns+
    ;; hydra
    ;; jinja2-mode
    ;; key-chord
    magit
    mu4e
    overseer
    ;; openwith
    org
    ;; (org-ref :location local)
    ;; ;; (otb :location local)
    ;; persistent-scratch
    ;; pos-tip
    ;; ;; request
    ;; s
    ;; sx
    ;; typescript
    )
  "List of all packages to install and/or initialize.
Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar joe-excluded-packages '()
  "List of packages to exclude.")

(defun joe/post-init-auto-yasnippet ()
  "Init auto-yasnippet."
  (use-package auto-yasnippet
    :config
    (progn
      (setq aya-persist-snippets-dir "~/.dotfiles/snippets")
      (add-to-list 'yas-snippet-dirs "~/.dotfiles/snippets")
      (setq yas-snippet-dirs (delete "~/.emacs.d/snippets" yas-snippet-dirs))
      (setq yas-snippet-dirs (delete
                              (expand-file-name "~/.emacs.d/private/snippets/")
                              yas-snippet-dirs))
      (yas-reload-all))))

(defun joe/init-camcorder ()
  (use-package camcorder
    :config
    (progn)))

(defun joe/init-doc-popup ()
  "Init doc-popup."
  (use-package doc-popup
    :config
    (progn
      (defvar evil-normal-state-map)
      (define-key evil-normal-state-map "gh" 'doc-popup-show-at-point))))

(defun joe/init-ebib ()
  "Init ebib."
  (use-package ebib))

(defun joe/init-framemove ()
  "Init framemove."

  (framemove-default-keybindings)
  (setq framemove-hook-into-windmove t))

(defun joe/init-helm-bibtex ()
  "Init helm-bibtex."
  (use-package helm-bibtex
    :defer t))

(defun joe/init-key-chord ()
  "Init key-chord."
  (use-package key-chord))

(defun joe/init-jinja2-mode ()
  "Init jinja2-mode."
  (use-package jinja2
    :defer t
    :init
    (progn
      (defun my-jinja2-block (id action context)
        (insert " ")
        (save-excursion
          (insert " ")))

      (require 'smartparens)
      (defvar sp-navigate-consider-stringlike-sexp)
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
  "Init evil."
  (use-package evil
    :config
    (progn
      (eval-when-compile
        (require 'evil-macros))

      (evil-define-motion my:evil-next-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-next-visual-line (* 5 (or count 1)))))

      (evil-define-motion my:evil-previous-visual-line-5 (count)
        "Move the cursor 5 lines up."
        :type line
        (let (line-move-visual)
          (evil-previous-visual-line (* 5 (or count 1)))))

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
  "Init evil-escape."
  (use-package evil-escape
    :config
    (progn
      (setq evil-escape-unordered-key-sequence t))))

(defun joe/init-hydra ()
  (use-package hydra
    :config
    (progn
      )))

(defun joe/post-init-org ()
  "Init org."
  (use-package org
    :config
    (progn

      (load "~/.dotfiles/jmacs/joe/local/my-org.el")
      )))

(defun joe/init-persistent-scratch ()
  "Init persistent-scratch."
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

(defun joe/init-typescript-mode ()
  "Init typescript-mode."
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

(defun joe/post-init-magit ()
  "Init magit."
  (use-package magit
    :config
    (progn
      (require 'smerge-mode)
      (setq smerge-refine-ignore-whitespace nil))))

(defun joe/post-init-mu4e ()
  (use-package mu4e
    :config
    (progn
      (require 'smtpmail)
      (require 'org-mu4e)

      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials (expand-file-name "~/.netrc.gpg")
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      (setq user-mail-address "joe.schafer@delta46.us"
            user-full-name  "Joe Schafer"
            message-signature
            (concat
             "Joe Schafer"
             "\n"))

      ;; Set default options which we customize per account in
      ;; `mu4e-account-alist'
      (setq mu4e-maildir "~/.mail"
            mu4e-trash-folder "/joesmoe10/trash"
            mu4e-refile-folder "/joesmoe10/archive"
            mu4e-get-mail-command "mbsync -a"
            mu4e-update-interval nil
            mu4e-compose-signature-auto-include nil
            mu4e-view-show-images t
            mu4e-view-show-addresses t)

      (setq mu4e-account-alist
            '(("joesmoe10"
               ;; About me
               (user-full-name "Joe Schafer")
               (user-mail-address "joesmoe10@gmail.com")
               (mu4e-compose-signature "--\nJoe Schafer")

               ;; Under each account, set the account-specific variables you want.
               (mu4e-sent-messages-behavior delete)
               (mu4e-sent-folder "/joesmoe10/sent")
               (mu4e-refile-folder "/joesmoe10/archive")
               (mu4e-drafts-folder "/joesmoe10/drafts")

               ;; SMTP
               (smtpmail-stream-type starttls)
               (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
               (smtpmail-smtp-user "joesmoe10")
               (smtpmail-smtp-server "smtp.gmail.com")
               (smtpmail-smtp-service 587))

              ("delta46"
               ;; About me
               (user-full-name "Joe Schafer")
               (user-mail-address "joe.schafer@delta46.us")
               (mu4e-compose-signature "--\nJoe Schafer")

               ;; Under each account, set the account-specific variables you want.
               (mu4e-sent-messages-behavior delete)
               (mu4e-sent-folder "/delta46/sent")
               (mu4e-refile-folder "/delta46/archive")
               (mu4e-drafts-folder "/delta46/drafts")

               ;; SMTP
               (smtpmail-stream-type starttls)
               (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
               (smtpmail-smtp-user "joe.schafer@delta46.us")
               (smtpmail-smtp-server "smtp.gmail.com")
               (smtpmail-smtp-service 587))))

      (setq mu4e-maildir-shortcuts
            '(("/joesmoe10/inbox" . ?i)
              ("/delta46/inbox" . ?I)
              ("/joesmoe10/archive" . ?e)
              ("/delta46/archive" . ?E)))

      (require 'mu4e-contrib)

      ;; Pandoc and html2markdown interpret HTML tables literally which is less
      ;; than ideal.  `mu4e-shr2text' works well, but is a bit too literal.
      (setq mu4e-html2text-command "html2markdown --bypass-tables --ignore-links")
      (setq mu4e-html2text-command "w3m -T text/html")
      (setq mu4e-html2text-command 'mu4e-shr2text)

      (setq mu4e-html2text-command "html2text -style pretty")

      (setq mu4e-change-filenames-when-moving t)

      (mu4e/mail-account-reset))))


(defun joe/init-otb ()
  "Init otb."
  (use-package otb
    :config
    (progn
      (joe/set-leader-keys
       "cb" 'joe-blog-compile
       "cB" '(lambda () (interactive) (joe-blog-compile 'force))
       "cp" 'joe-blog-publish
       "cP" 'joe-blog-purge-everything))
    ))

(defun joe/init-org-ref ()
  "Init org-ref."
  (use-package org-ref
    :config
    (progn
      ;; optional but very useful libraries in org-ref
      ;; (require 'doi-utils)
      ;; (require 'jmax-bibtex)
      ;; (require 'pubmed)
      ;; (require 'arxiv)
      ;; (require 'sci-id)
      ;; (require 'bibtex)
      ;; (require 'reftex-cite)
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

(defun joe/init-openwith ()
  (use-package openwith
    :config
    (progn
      (setq openwith-associations
            (list
             '("\\.pdf\\'" "zathura" (file))))
      )))

(defun joe/init-overseer ()
  (use-package overseer
    :config
    (progn
      (add-to-list 'exec-path (expand-file-name "~/.cask/bin"))
              )))

(defun joe/post-init-s ()
  "Init s ()."
  (use-package s
    :config
    ))

(defun joe/init-sx ()
  (use-package sx
    :config
    (progn
      (evil-set-initial-state 'sx-question-mode 'emacs)
      (evil-set-initial-state 'sx-question-list-mode 'emacs)
      (evil-leader/set-key "xss" 'sx-search)
      )))

(provide 'packages)
;;; packages.el ends here
