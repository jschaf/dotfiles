;;; abn-module-email.el --- Config for email

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-email
  :ensure nil ; local package
  :commands (abn/mu4e-set-account)
  )

(use-package mu4e
  :ensure nil ; Installed with package manager
  :defer t
  :config

  ;; Set inital value.  Will be overridden by `mu4e-account-alist'.
  (setq
   mu4e-maildir       "~/mail"
   ;; Folder names relative to `mu4e-maildir'.
   mu4e-sent-folder   "/joesmoe10/sent"
   mu4e-drafts-folder "/joesmoe10/drafts"
   mu4e-trash-folder  "/joesmoe10/trash"
   mu4e-refile-folder "/joesmoe10/archive")


  (setq abn-mu4e-account-alist
        '(("joesmoe10"
           ;; User info.
           (user-full-name "Joe Schafer")
           (user-mail-address "joesmoe10@gmail.com")
           (mu4e-compose-signature "--\nJoe Schafer")

           ;; Account-specific variables.
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
           ;; User info.
           (user-full-name "Joe Schafer")
           (user-mail-address "joe.schafer@delta46.us")
           (mu4e-compose-signature "--\nJoe Schafer")

           ;; Account-specific variables.
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

  (setq mu4e-change-filenames-when-moving t)
  (add-hook 'mu4e-compose-pre-hook 'abn/mu4e-set-account))




(provide 'abn-module-email)
;;; abn-module-email.el ends here
