;;; evil-unimpaired.el --- Pairs of handy bracket mappings.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: evil, vim-unimpaired, spacemacs
;; Version: 0.1
;; Package-Requires: ((dash "2.12.0") (f "0.18.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a port of vim-unimpaired https://github.com/tpope/vim-unimpaired
;; `evil-unimpaired' provides pairs of handy bracket mappings to quickly navigate
;; to previous/next thing and more.

;;; Code:

(require 'dash)
(require 'f)

(defun evil-unimpaired//find-relative-filename (offset)
  (when buffer-file-name
    (let* ((directory (f-dirname buffer-file-name))
           (files (f--files directory (not (s-matches? "^\\.?#" it))))
           (index (+ (-elem-index buffer-file-name files) offset))
           (file (and (>= index 0) (nth index files))))
      (when file
        (f-expand file directory)))))

;;;###autoload
(defun evil-unimpaired/previous-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename -1))
      (find-file filename)
    (user-error "No previous file")))

;;;###autoload
(defun evil-unimpaired/next-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename 1))
      (find-file filename)
    (user-error "No next file")))

;;;###autoload
(defun evil-unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

;;;###autoload
(defun evil-unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

;;;###autoload
(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

;;;###autoload
(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

;;;###autoload
(defun evil-unimpaired/next-frame ()
  (interactive)
  (raise-frame (next-frame)))

;;;###autoload
(defun evil-unimpaired/previous-frame ()
  (interactive)
  (raise-frame (previous-frame)))

(provide 'evil-unimpaired)
;;; evil-unimpaired.el ends here.
