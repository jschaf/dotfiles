;;; abn-funcs-email.el --- Functions for email

;;; Commentary:
;;

;;; Code:

(defun abn/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                abn-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) abn-mu4e-account-alist)
                             nil t nil nil (caar abn-mu4e-account-alist))))
         (account-vars (cdr (assoc account abn-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(provide 'abn-funcs-email)
;;; abn-funcs-email.el ends here
