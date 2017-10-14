;;; packages.el --- joe Layer packages File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org


;;; Commentary:
;;

;;; Code:

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

(defun joe/post-init-avy ()
  "Init avy."
  (use-package avy
    :config
    (progn

      (defun my:avy-back-to-start ()
        "Go back to where we invoked avy.
This is par tof avy-action-copy, so that function doesn't need it."
        (let ((dat (ring-ref avy-ring 0)))
          (select-frame-set-input-focus
           (window-frame (cdr dat)))
          (select-window (cdr dat))
          (goto-char (car dat))))

      (defun my:avy-action-copy-and-yank (pt)
        "Copy and yank sexp starting on PT."
        (avy-action-copy pt)
        (yank))

      (defun my:avy-action-copy-and-yank-line (pt)
        "Copy and yank the whole line on PT."
        (kill-new (buffer-substring (line-beginning-position) (line-end-position)))
        (my:avy-back-to-start)
        (save-excursion
          (beginning-of-line)
          (insert "\n")
          (forward-char -1)
          (yank)))
      ;; Perform actions on avy targets.  Press one of the following chars
      ;; before selecting a target.  For example if you want to delete the line
      ;; with the `fg' target, `avy-goto-line x fg'.
      ;; http://emacs.stackexchange.com/questions/27979
      (setq avy-dispatch-alist
            '((?x . avy-action-kill-move)
              (?X . avy-action-kill-stay)
              (?m . avy-action-mark)
              (?i . avy-action-ispell)
              (?p . my:avy-action-copy-and-yank)
              (?P . my:avy-action-copy-and-yank-line)
              (?n . avy-action-copy)
              ))
      ))
  )

(defun joe/init-bbdb ()
  (use-package bbdb
    :config
    (progn
      (setq bbdb-file "~/gdrive/contacts/bbdb")

      (defvar my:bbdb-asynk-host "gc_joesmoe10")
      (defvar my:bbdb-asynk-name "joesmoe10")
      (defvar my:bbdb-asynk-path (file-truename "~/prog/ASynK/asynk.py"))
      (defvar my:bbdb-asynk-user-dir (file-truename "~/.asynk"))

      (defun my:bbdb-asynk-sync ()
        "Sync bbdb with ASynK."
        (interactive)
        (require 'netrc)
        (with-temp-buffer
          (let* ((netrc (netrc-parse "~/.netrc.gpg"))
                 (hostentry (netrc-machine netrc my:bbdb-asynk-host))
                 (default-directory my:bbdb-asynk-user-dir))
            (unless hostentry (error "Could not find %s in ~/.netrc.gpg"
                                     my:bbdb-asynk-host))
            (message "Running AsynK...")
            (insert (netrc-get hostentry "login")
                    "\n"
                    (netrc-get hostentry "password")
                    "\n")
            (let ((proc (start-process "ASynK" "*ASynK*"
                                       "python2" my:bbdb-asynk-path
                                       "--op" "sync"
                                       "--user-dir" my:bbdb-asynk-user-dir
                                       "--name" my:bbdb-asynk-name)))
              (set-process-sentinel proc (lambda (p s)
                                           (if (equal s "finished\n")
                                               (message "ASynK %s" s)
                                             (display-buffer (process-buffer p))
                                             (error "ASynK: %s" s))))
              (insert (netrc-get hostentry "login")
                      "\n"
                      (netrc-get hostentry "password")
                      "\n")
              (with-current-buffer (process-buffer proc)
                (erase-buffer))
              ;; (display-buffer (process-buffer proc))
              (process-send-region proc (point-min) (point-max))
              (process-send-eof proc)
              (or (not (buffer-live-p bbdb-buffer))
                  (bbdb-revert-buffer nil 'noconfirm))))))

      (with-eval-after-load 'mu4e
        (advice-add 'mu4e-quit :after 'my:bbdb-asynk-sync))

      )))

(defun joe/post-init-company ()
  "Init company."
  (use-package company
    :config
    (progn
      (defun my:complete-with-dot-and-complete-again ()
        "Complete word at point, insert a period and complete again."
        (interactive)
        (company-complete-selection)
        (insert ".")
        (company-complete-common))

      (define-key company-active-map (kbd "C-.")
        'my:complete-with-dot-and-complete-again))))

(defun joe/init-evil-terminal-cursor-changer ()
  "Init it."
  (use-package evil-terminal-cursor-changer
    :if (not (my:is-tty))
    :init
    (progn
      (unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        ))))

(defun joe/post-init-flycheck ()
  "Post init flycheck."

  (use-package flycheck
    :config
    (progn
      (flycheck-define-checker markdown-markdownlint
                               "Markdown checker using mdl.

See URL `https://github.com/mivok/markdownlint'."
                               :command ("markdownlint" source)
        ;; :standard-input t
                               :error-patterns
        ((error line-start
                (file-name) ": " line ": " (id (one-or-more alnum)) " " (message)
                line-end))

        :modes (markdown-mode gfm-mode))

      (add-to-list 'flycheck-checkers 'markdown-markdownlint))))

(defun joe/post-init-org ()
  "Init org."
  (use-package org
    :config
    (progn

      (load "~/.dotfiles/layers/joe/local/my-org.el")
      (when (file-exists-p "~/.dotfiles/layers/joe/local/buggy.el")
        (load "~/.dotfiles/layers/joe/local/buggy.el"))
      )))

(defun joe/post-init-org-agenda ()
  "Init org-agenda."
  (use-package org-agenda
    :config
    (progn
      (defun my:org-agenda-refile-to-goog ()
        (interactive)
        (org-agenda-refile nil
                           '("gtasks/ (goog.org)"
                             "~/gdrive/gorg/goog.org"
                             "^\\(\\*+\\)\\(?: +\\(CANCELLED\\|DONE\\|HOLD\\|NEXT\\|TODO\\|WAITING\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(?:\\[[0-9%/]+\\] *\\)*\\(Work Tasks\\)\\(?: *\\[[0-9%/]+\\]\\)*\\)\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$"
                             1)))

      (defun my:org-agenda-refile-to-gtd ()
        (interactive)
        (org-agenda-refile nil
                           ("htasks/ (gtd.org)"
                            "/Users/jschaf/gdrive/org/gtd.org"
                            "^\\(\\*+\\)\\(?: +\\(CANCELLED\\|DONE\\|HOLD\\|NEXT\\|TODO\\|WAITING\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(?:\\[[0-9%/]+\\] *\\)*\\(Tasks\\)\\(?: *\\[[0-9%/]+\\]\\)*\\)\\(?:[ 	]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[ 	]*$"
                            179)))
      (spacemacs/set-leader-keys-for-major-mode
        'org-agenda-mode
        "rr" 'org-agenda-refile
        "rg" 'my:org-agenda-refile-to-gtd
        "rw" 'my:org-agenda-refile-to-goog
        ))))

(defun joe/init-org-autolist ()
  "Init org-autolist."
  (use-package org-autolist
    :config
    (progn
      (add-hook 'org-mode-hook 'org-autolist-mode)
      ))
  )

(defun joe/post-init-org-download ()
  "Init org-download."
  (use-package org-download
    :config
    (progn
      (setq-default org-download-image-dir "~/gdrive/org/images"))))

(defun joe/post-init-mu4e ()
  "Init mu4e."
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

      ;;send mail using postfix
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail)

      (setq mu4e-maildir-shortcuts
            '(("/joesmoe10/inbox" . ?i)
              ("/delta46/inbox" . ?I)
              ("/joesmoe10/archive" . ?e)
              ("/delta46/archive" . ?E)))

      (require 'mu4e-contrib)

      ;; Pandoc and html2markdown interpret HTML tables literally which is less
      ;; than ideal.  `mu4e-shr2text' works well, but is a bit too literal.
      ;; (setq mu4e-html2text-command "html2markdown --bypass-tables --ignore-links")
      ;; (setq mu4e-html2text-command "w3m -T text/html")
      ;; (setq mu4e-html2text-command 'mu4e-shr2text)
      ;; (setq mu4e-html2text-command "html2text -utf8 -width 72 -style pretty")
      (setq mu4e-html2text-command "html2text -nobs -utf8")

      (setq mu4e-change-filenames-when-moving t)
      (require 'bbdb)
      (setq bbdb-mail-user-agent 'message-user-agent)
      (add-to-list 'mu4e-view-mode-hook #'bbdb-mua-auto-update)
      (add-to-list 'mu4e-view-mode-hook #'visual-line-mode)
      (setq mu4e-compose-complete-addresses nil)
      (setq bbdb-mua-pop-up t)
      (setq bbdb-mua-pop-up-window-size 5)

      (defun my:mu4e-set-account ()
        "Set the account for composing a message."
        (let* ((account
                (if mu4e-compose-parent-message
                    (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                      (string-match "/\\(.*?\\)/" maildir)
                      (match-string 1 maildir))
                  (completing-read (format "Compose with account: (%s) "
                                           (mapconcat #'(lambda (var) (car var))
                                                      mu4e-account-alist "/"))
                                   (mapcar #'(lambda (var) (car var)) mu4e-account-alist)
                                   nil t nil nil (caar mu4e-account-alist))))
               (account-vars (cdr (assoc account mu4e-account-alist))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

      (add-hook 'mu4e-compose-pre-hook 'my:mu4e-set-account)

      (mu4e/mail-account-reset))))

(defun joe/post-init-racer ()
  "Init racer."
  (use-package racer
    :config
    (exec-path-from-shell-copy-env "RUST_SRC_PATH")))

(defun joe/post-init-smartparens ()
  "Init smartparens."
  (use-package smartparens
    :config
    (spacemacs/set-leader-keys
     "k C-h" #'sp-beginning-of-sexp
     "k C-l" #'sp-beginning-of-next-sexp)
    (sp-with-modes 'org-mode
      (sp-local-pair "~" "~"))
    ))

(defun joe/post-init-web-mode ()
  "Init web-mode."
  (use-package web-mode
    :config
    (progn
      (add-hook 'web-mode-hook 'turn-off-show-smartparens-mode))))

(defun joe/post-init-zeal-at-point ()
  "Init zeal-at-point."
  (use-package zeal-at-point
    :config
    (progn
      (defun my:pick-docset ()
        "Choose custom docsets based on the buffer."
        (setq zeal-at-point-docset
              (cond
               ((equal major-mode 'js2-mode) "goog,javascript,angularjs")
               (t nil))))
      (add-hook 'prog-mode-hook #'my:pick-docset))))

(provide 'packages)
;;; packages.el ends here
