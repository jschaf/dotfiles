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
    (doc-popup :location local)
    ;; ebib
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
    (org-drill :location built-in)
    (ox-publish :location built-in)
    ;; (org-ref :location local)
    ;; ;; (otb :location local)
    ;; pos-tip
    ;; ;; request
    ;; s
    smartparens
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
  (use-package framemove
    :config
    (progn
      (framemove-default-keybindings)
      (setq framemove-hook-into-windmove t))))

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

(defun joe/init-org-drill ()
  "Init org-drill."
  (use-package org-drill--edit-key
    :config

    (defun my:work-around-org-window-drill-bug ()
      "Comment out a troublesome line in `org-toggle-latex-fragment'.
See https://bitbucket.org/eeeickythump/org-drill/issues/30 for
details."
      (save-excursion
        (let ((org-library-location (concat
                                     (locate-library "org" 'nosuffix)
                                     ".el")))
          (with-current-buffer (find-file-noselect org-library-location)
            (goto-char (point-min))
            (search-forward "(set-window-start nil window-start)")
            (back-to-indentation)
            (if (looking-at ";; ")
                (message "Already modified `org-toggle-latex-fragment' for `org-drill'")
              (insert ";; ")
              (save-buffer)
              (byte-compile-file org-library-location)
              (elisp--eval-defun)
              (message "Modified `org-toggle-latex-fragment' for `org-drill'"))))))

    (my:work-around-org-window-drill-bug)

    (defun my:org-set-tag-as-drill ()
      "Set the current headline as a drill tag."
      (interactive)
      (org-toggle-tag "drill"))

    (defun my:org-drill-create-template ()
      "Insert a snippet for a new drill item."
      (interactive)
      (insert "*** Item                                      :drill:\n\n")
      (insert "Question\n\n")
      (insert "**** Answer\n\n")
      (insert "Answer\n")
      (search-backward "Item")
      (forward-word)
      (forward-char))

    (defun my:org-drill-create-template-cloze ()
      "Insert a template for cloze."
      (interactive)
      (insert "*** Item                                      :drill:\n")
      (insert ":PROPERTIES:\n:DRILL_CARD_TYPE: hide1cloze\n:END:\n\n")
      (insert "[Question] and [Answer]\n\n")
      (search-backward "Item")
      (forward-word)
      (forward-char))
    (joe/set-leader-keys
     "dd" 'my:org-set-tag-as-drill
     "dt" 'my:org-drill-create-template
     "dc" 'my:org-drill-create-template-cloze)
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
  "Init overseer."
  (use-package overseer
    :config
    (progn
      (add-to-list 'exec-path (expand-file-name "~/.cask/bin"))
      )))

(defun joe/init-ox-publish ()
  "Init ox-publish."
  (use-package ox-publish
    :config
    (progn
      (dolist (project
               `(("swift-plaques"
                  :author "Joe Schafer"
                  :base-directory "~/prog/swift-plaques-business-plan"
                  :publishing-directory "~/prog/swift-plaques-business-plan"
                  :publishing-function org-latex-publish-to-pdf
                  :base-extension "org"
                  )))
        (my:replace-or-add-to-alist 'org-publish-project-alist project))

      (joe/set-leader-keys
       "cs" 'swift-plaques-compile)
      )))

(defun joe/post-init-s ()
  "Init s ()."
  (use-package s
    :config
    ))

(defun joe/post-init-smartparens ()
  "Init s ()."
  (use-package s
    :config
    (spacemacs/set-leader-keys
      "k C-h" #'sp-beginning-of-sexp
      "k C-l" #'sp-beginning-of-next-sexp)
    ))

(defun joe/init-sx ()
  "Init sx."
  (use-package sx
    :config
    (progn
      (evil-set-initial-state 'sx-question-mode 'emacs)
      (evil-set-initial-state 'sx-question-list-mode 'emacs)
      (evil-leader/set-key "xss" 'sx-search)
      )))

(provide 'packages)
;;; packages.el ends here
