;;; abn-funcs-benchmark.el --- Functions for benchmark

;;; Commentary:
;;

;;; Code:


(defvar abn--loaded-file-paths nil
  "All file paths that are loaded.")

(defvar abn--loaded-packages-buffer "*loaded-packages*"
  "Buffer name for data about loaded packages.")

(defvar abn--loaded-features-buffer "*loaded-features*"
  "Buffer name for data about loaded features.")

(defun abn/list-loaded-packages()
  "List all currently loaded file paths."
  (interactive)
  (with-current-buffer (get-buffer-create abn--loaded-packages-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    ;; Extract data from builtin variable `load-history'.
    (setq abn--loaded-file-paths
          (seq-filter #'stringp
                      (mapcar #'car load-history)))
    (cl-loop for file in abn--loaded-file-paths
             do (insert "\n" file))

    (insert "\n\n* Live Packages Exploration\n\n")
    (insert (format "%s total packages currently loaded\n\n"
                    (length abn--loaded-file-paths)))))

(defun abn/list-loaded-features()
  "List all currently loaded features."
  (interactive)
  (with-current-buffer (get-buffer-create abn--loaded-features-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    (insert (format "\n** %d features currently loaded\n"
                    (length features)))

    (dolist (x features)
      (insert (format "  - %s: %s\n" x (locate-library (symbol-name x)))))

    (goto-char (point-min))))

(provide 'abn-funcs-benchmark)
;;; abn-funcs-benchmark.el ends here
