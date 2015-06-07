(defvar my:mac-modifier-state 'mac
  "Toggle between BUILT-IN and USB")

(defun my:toggle-mac-modifiers ()
  (interactive)
  (if (eq my:mac-modifier-state 'usb)
      (progn
        (setq my:mac-modifier-state 'built-in
              mac-option-modifier 'control
              mac-command-modifier 'meta)
        (message "Mac modifier keys set for Mac keyboard."))

    (setq my:mac-modifier-state 'usb
          mac-option-modifier 'meta
          mac-command-modifier 'super)
    (message "Mac modifier keys set for USB keyboard.")))

(my:toggle-mac-modifiers)

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

(defmacro my:make-evil-line-move-motion (name multiplier)
  `(evil-define-motion ,name (count)
     ,(format "Move the cursor (COUNT * %s) lines down." multiplier)
     :type line
     (let (line-move-visual)
       (evil-next-visual-line (* ,multiplier (or count 1))))))

(my:make-evil-line-move-motion my:evil-next-visual-line-5 5)
(my:make-evil-line-move-motion my:evil-previous-visual-line-5 -5)
(my:make-evil-line-move-motion my:evil-next-visual-line-3 3)
(my:make-evil-line-move-motion my:evil-previous-visual-line-3 -3)


(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))



(defun my:create-subtle-show-paren-match ()
  (interactive)
  (set-face-attribute 'show-paren-match nil
                      :foreground (face-foreground 'default)
                      :weight 'normal
                      :background (my:differentiate-color
                                   (face-background 'default) 15)))

;; (after 'paren
;;   (add-hook 'my:load-theme-hook 'my:create-subtle-show-paren-match)
;;   (my:create-subtle-show-paren-match))


(defun my:yank-sexp ()
  "Yank the sexp in front of the point."
  (interactive)
  (save-excursion
    (mark-sexp)
    (kill-ring-save (point) (mark))))

(defun info-mode ()
  "A simple function to open standalone info files correctly."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

(defun my:last-non-blank-or-end-of-line ()
  "Go to last non blank, or end of line on second press."
  (interactive)
  (if (eq last-command 'my:last-non-blank-or-end-of-line)
      (evil-end-of-line)
    (evil-last-non-blank)))

(defun my:hungry-delete-backward (n &optional killflag)
  "Delete non-vertical whitespace backwards on first key press.
Delete all whitespace on a succesive key press."
  (interactive "p\nP")
  (if (eq last-command 'my:hungry-delete-backward)
      (hungry-delete-backward n killflag)
    (let ((hungry-delete-chars-to-skip " \t\f\v"))
      (hungry-delete-backward n killflag))))

(defun my:maybe-byte-compile ()
  "Byte compile current file if .elc file exists."
  (interactive)
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (byte-compile-file buffer-file-name)))

(defun my:maybe-byte-compile-after-save ()
  "Add `my:maybe-byte-compile' to the local `after-save-hook'."
  (add-hook (make-local-variable 'after-save-hook) 'my:maybe-byte-compile))

(defun my:toggle-compile-on-save ()
  "Add `compile' to the local `after-save-hook'."
  (interactive)
  (if (memq 'recompile after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'recompile 'local)
        (message "%s will NOT compile when saved" (buffer-name)))
    (add-hook 'after-save-hook 'recompile nil 'local)
    (message "%s will compile when saved" (buffer-name))))

(defun my:delete-trailing-whitespace-except-current-line ()
  "Do `delete-trailing-whitespace', except for current line."
  (interactive)
  (let ((current-line (buffer-substring (line-beginning-position)
                                        (line-end-position)))
        (backward (- (line-end-position) (point)))
        (newlines (looking-back "\n\n" (- (point) 2 ))))
    (delete-trailing-whitespace)
    (when (not (string-equal (buffer-substring (line-beginning-position)
                                               (line-end-position))
                             current-line))
      (delete-region (line-beginning-position) (line-end-position))
      (insert current-line)
      (backward-char backward))
    ;; It's annoying to add a couple newlines to the end of a file and
    ;; hit save and then watch them disappear.
    (when (and (eobp) newlines)
        (insert "\n"))))

(defun my:delete-trailing-whitespace-before-save ()
  "Add `delete-trailing-whitespace' to the local `after-save-hook'."
  (add-hook (make-local-variable 'before-save-hook)
            'my:delete-trailing-whitespace-except-current-line))

(defun my:new-blah-buffer ()
  "Open up a guaranteed new blah (scratch) buffer."
  (interactive)
  (switch-to-buffer (cl-loop for num from 0
                          for name = (format "blah-%03i" num)
                          while (get-buffer name)
                          finally return name)))

(defun my:switch-to-blah-buffer ()
  "Switch to a blah buffer, or create a new one."
  (interactive)
  (cl-loop for buffer in (buffer-list)
      if (string-match "blah-.+" (buffer-name buffer))
         return (switch-to-buffer buffer)
      finally do (my:new-blah-buffer)))
