;;; abn-help-funcs.el --- Functions for help related tools

;;; Commentary:
;;

(defun abn//describe-last-keys-string ()
  "Gathers info about your Emacs last keys and returns it as a string."
  (loop
   for key
   across (recent-keys)
   collect (if (or (integerp key) (symbolp key) (listp key))
               (single-key-description key)
             (prin1-to-string key))
   into keys
   finally (return
            (with-temp-buffer
              (set-fill-column 60)
              (insert (mapconcat 'identity keys " "))
              (fill-region (point-min) (point-max))
              (format "#### Emacs last keys :musical_keyboard: \n```text\n%s\n```\n" (buffer-string))))))

(defun abn/describe-last-keys ()
  "Gathers info about your Emacs last keys and copies to clipboard."
  (interactive)
  (let ((lossage (abn//describe-last-keys-string)))
    (kill-new lossage)
    (message (concat "Information copied to clipboard:\n" lossage))))

(defun abn//describe-system-info-string ()
  "Gathers info about your setup and returns it as a string."
  (format
   (concat "#### System Info :computer:\n"
           "- OS: %s\n"
           "- Emacs: %s\n"
           "- Graphic display: %s\n"
           (when (version<= "25.1" emacs-version)
             "- System configuration features: %s\n"))
   system-type
   emacs-version
   (display-graphic-p)
   (bound-and-true-p system-configuration-features)))

(defun abn/describe-system-info ()
  "Gathers info about your setup and copies to clipboard."
  (interactive)
  (let ((sysinfo (abn//describe-system-info-string)))
    (kill-new sysinfo)
    (message (concat "Copied to clipboard:\n" sysinfo))))

(provide 'abn-help-funcs)

;;; abn-help-funcs.el ends here
