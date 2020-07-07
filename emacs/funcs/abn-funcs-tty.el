;;; abn-funcs-tty.el --- Functions for tty

;;; Commentary:
;;

;;; Code:

(defun abn-tty/yank (text)
  "Yank TEXT to the clipboard from a terminal Emacs."
  (if window-system
      (error "Trying to copy text in a GUI emacs.")
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max)
                           (expand-file-name abn-dotfiles-dir "bin/clipboard-copy")))))

(defun abn-tty/buffer-substring-terminal-filter (beg end &optional delete)
  "A filter that uses the default filter but also adds text to clipboard."
  (let ((result (buffer-substring--filter beg end delete)))
    ;; Only copy sizable entries to avoid spamming the clipboard.
    (when (> (length result) 4)
      (osc52-select-text (buffer-substring beg end))
      ;; (abn-tty/yank result)
      )
    result))

(defun abn-tty/refresh-cursor ()
  "Refresh the cursor in Emacs.
Useful when it gets stale automagically changed by ZSH."
  (when (fboundp 'etcc--evil-set-cursor)
    (etcc--evil-set-cursor)))

(provide 'abn-funcs-tty)
;;; abn-funcs-tty.el ends here
