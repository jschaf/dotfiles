;;; buggy.el --- bug killing process

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(defvar buggy-template-path
  "~/gdrive/org-templates/buggy.org"
  "Path to the org file that represents a buggy template.")

(defvar buggy-slice-template-path
  "~/gdrive/org-templates/slice.org"
  "Path to the org file that represents a slice template.")

(defun buggy-get-file-contents-as-string (path)
  "Return a string of the contents of PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun buggy-get-buggy-string ()
  "Get a string of the buggy.org template."
  (buggy-get-file-contents-as-string buggy-template-path))

(defun buggy-insert-buggy ()
  "Insert a buggy template at the current point."
  (interactive)
  (insert (buggy-get-buggy-string)))

(defun buggy-get-slice-string ()
  "Get a string of the buggy.org template."
  (buggy-get-file-contents-as-string buggy-slice-template-path))

(defun buggy-get-nearest-buggy-level ()
  "Return the level of the heading of the nearest :buggy:."
  1)

(defun buggy-demote-slice-to-level (slice-string buggy-level)
  "Make a SLICE-STRING a child of under a heading at BUGGY-LEVEL."
  (with-temp-buffer
    (org-mode)
    (insert slice-string)
    (goto-char (point-min))
    (cl-loop for level from 0 below buggy-level
             do
             (org-demote-subtree))
    (buffer-string)))

(defun buggy-insert-slice ()
  "Insert a slice template at the current point."
  (interactive)
  (let* ((slice (buggy-get-slice-string))
         (demoted-slice
          (buggy-demote-slice-to-level slice
                                       (buggy-get-nearest-buggy-level))))
    (insert demoted-slice)))

(provide 'buggy)
;;; buggy.el ends here
