;;; abn-funcs-editing.el --- Functions for editing text

;;; Commentary:
;;

(defun abn/evil-goto-next-line-and-indent (&optional count)
  "Match the current lines indentation to the next line.
A COUNT argument matches the indentation to the next COUNT lines."
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

(defvar abn-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defun abn/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode abn-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun abn/shell-command-on-buffer (command &optional keep-buffer-p)
  "Prompt and run a COMMAND on the buffer.
By default, `shell-command-on-buffer' will replace the contents
of the buffer with the output of COMMAND.  If KEEP-BUFFER-P is
non-nil, keep the original buffer content."
  (interactive (list (read-shell-command "Shell command on buffer: ")))
  (shell-command-on-region
   (point-min) (point-max)
   command
   'use-current-buffer
   (not keep-buffer-p)))

(defun abn/split-and-new-line ()
  "Split a quoted string or s-expression and insert a new line with
auto-indent."
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(provide 'abn-funcs-editing)

;;; abn-funcs-editing.el ends here
