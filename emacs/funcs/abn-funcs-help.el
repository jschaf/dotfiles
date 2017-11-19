;;; abn-funcs-help.el --- Functions for help related tools

;;; Commentary:
;;

(defun abn/describe-keymap (keymap)
  "Describe key bindings in KEYMAP.
Interactively, prompt for a variable that has a keymap value.

Non-interactively:
* KEYMAP can be such a keymap variable or a keymap."
  (interactive (list (abn//read-keymap)))
  (when (or (not (symbolp keymap))
            (not (boundp keymap))
            (not (keymapp (symbol-value keymap))))
    (error "Not a keymap"))

  (let ((aliased-keymap
         (or (condition-case nil (indirect-variable keymap) (error nil))
             keymap)))
    (abn//display-keymap-help aliased-keymap)))

(defun abn//read-keymap ()
  "Read a keymap from the user."
  (intern
   (completing-read
    "Keymap: " obarray
    (lambda (m) (and (boundp m)  (keymapp (symbol-value m))))
    t nil 'variable-name-history)))

(defun abn//display-keymap-help (keymap)
  "Display a help buffer for KEYMAP."
  (let* ((name (symbol-name keymap))
         (was-interactive-p (called-interactively-p 'interactive))
         (keymap-doc (documentation-property keymap 'variable-documentation)))
    
    (help-setup-xref (list #'describe-keymap keymap) was-interactive-p)

    (with-help-window (help-buffer)
      (princ name) (terpri) (princ (make-string (length name) ?-)) (terpri) (terpri)

      (with-current-buffer "*Help*"
        ;; Use `insert' instead of `princ', so control chars
        ;; (e.g. \377) insert correctly.
        (insert (substitute-command-keys (concat "\\{" name "}")))))))

(defun abn/describe-system-info ()
  "Gathers info about your setup and copies to clipboard."
  (interactive)
  (let ((sysinfo (abn//describe-system-info-string)))
    (kill-new sysinfo)
    (message (concat "Copied to clipboard:\n" sysinfo))))


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

(provide 'abn-funcs-help)

;;; abn-funcs-help.el ends here
