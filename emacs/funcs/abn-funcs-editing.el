;;; abn-funcs-editing.el --- Functions for editing text

;;; Commentary:
;;

(defun abn/back-to-indentation-or-beginning ()
  "Move point to first non-whitespace char or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

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

(defun abn/goto-middle-of-line ()
  "Move cursor to middle of the line."
  (interactive)
  (goto-char
   (/
    (+ (line-end-position) (line-beginning-position))
    2)))

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

(defun abn/yank-file-path (&optional use-full-expansion)
  "Yanks the file path of the current buffer."
  (interactive "P")
  (let ((path (abn/get-current-buffer-file-path use-full-expansion)))
    (kill-new path)
    (message "%s" path)))

(defun abn/yank-directory-path (&optional use-full-expansion)
  "Yanks the file path of the current buffer."
  (interactive "P")
  (let ((path (file-name-directory (abn/get-current-buffer-file-path
                                    use-full-expansion))))
    (kill-new path)
    (message "%s" path)))

(defun abn/get-current-buffer-file-path (use-full-expansion)
  "Get the file path from the current buffer"
  (cond
   ((string-equal major-mode "dired-mode")
    (abn//get-dired-file-path use-full-expansion))
   ((eq (buffer-file-name) nil)
    nil)
   (t
    (abn//get-buffer-file-path use-full-expansion))))

(defun abn//get-dired-file-path (use-full-expansion)
  "Yanks the current file path from a dired buffer."
  default-directory)

(defun abn//get-buffer-file-path (use-full-expansion)
  "Yanks the current file path from the buffer."
  (let ((path (buffer-file-name)))
    (if use-full-expansion
        path
      (abbreviate-file-name path))))

(defun abn/yank-last-message ()
  "Yank the last message from the *Messages* buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    ;; Not wrapping in save excursion, since Emacs knows how to modify
    ;; the *Messages* buffer.
    (goto-char (point-max))
    ;; If there's an empty line at the end, that's probably not
    ;; what we want, so go up one line.
    (when (eq (point-max) (line-beginning-position))
      (forward-line -1))
    (let* ((last-line (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
           (max-snippet-length 35)
           (snippet (if (> (length last-line) max-snippet-length)
                        (concat
                         (substring last-line 0 max-snippet-length)
                         "...")
                      last-line)))
      (kill-new last-line)
      (message "Yanked message: \"%s\"" snippet))))

(provide 'abn-funcs-editing)
;;; abn-funcs-editing.el ends here
