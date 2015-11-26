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

(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun my:back-to-indentation-or-beginning ()
  "Go back to indentation, or beginning of line on second press."
  (interactive)
  (if (eq last-command 'my:back-to-indentation-or-beginning)
      (evil-beginning-of-line)
    (evil-first-non-blank)))

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
