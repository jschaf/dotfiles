;;; abn-module-debug.el --- Config for debug

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-debug
  :ensure nil ; local package
  )

(defvar abn-enable-timed-loads-p t
  "If non-nil, record timing information.")

(defvar abn-timed-load-threshold 0.1
  "Records all `require', `load' calls greater than this threshold.")

(defvar abn-timed-loads-buffer "*load-times*"
  "The buffer to use for timing information.")

(defsubst abn//debug-time-since-emacs-init ()
  "The time since Emacs started."
  (time-subtract (current-time) before-init-time))

(defsubst abn//debug-time-since-abn-init ()
  "The time since Emacs started eval'ing user code."
  (time-subtract (current-time) abn-init-time))

(defun abn//debug-log-timing (str &rest args)
  (with-current-buffer abn-timed-loads-buffer
    (goto-char (point-max))
    (insert
     "+"
     (format-time-string
      "%2s.%3N" (abn//debug-time-since-emacs-init))
     ": "
     (apply 'format str args))))

(with-current-buffer (get-buffer-create abn-timed-loads-buffer)
  (insert (format "Threshold set at %.3f seconds\n" abn-timed-load-threshold)
          "Emacs start time: "
          (format-time-string "%Y-%m-%d %H:%M:%S.%6N" before-init-time)
          "\n"
          "abn start time:   "
          (format-time-string "%Y-%m-%d %H:%M:%S.%6N" abn-init-time)
          "\n\n"))

(defun abn//record-time-with-desc (description orig-fun &rest args)
  "Records the time with DESCRIPTION for a function ORIG-FUN with ARGS."
  (let ((start (current-time)) result delta)
    (setq result (apply orig-fun args))
    (setq delta (float-time (time-since start)))
    (when (> delta abn-timed-load-threshold)
      (with-current-buffer abn-timed-loads-buffer
        (goto-char (point-max))
        (insert (format "%s %.3f sec\n" description delta))))
    result))

(defun abn//debug-require-timer (orig-fun &rest args)
  "Used to time invocation of `require' or `load'."
  (let ((start (current-time))
        (required (car args))
        delta)

    (prog1
        (apply orig-fun args)

      (setq delta (float-time (time-since start)))
      (when (> delta abn-timed-load-threshold)
        (abn//debug-log-timing
         "%.3f func=%s feature=%s file=%s\n"
         delta orig-fun required load-file-name)))))

(defun abn//debug-time-package-initialize (orig-fun &rest args)
  "Record timing information for `package-initialize'."
  (abn//record-time-with-desc "package-initialize:" orig-fun args))

(defun abn//debug-time-require (orig-fun &rest args)
  "Record timing information for `require'."
  (let ((load-file load-file-name)
        (require-symbol (car args)))
    (abn//record-time-with-desc
     (format "require: symbol=%s load-file=%s" require-symbol load-file)
     orig-fun args)))

(defun abn//debug-time-load (orig-fun &rest args)
  "Record timing information for `load'."
  (let ((load-file load-file-name)
        (require-symbol (car args)))
    (abn//record-time-with-desc
     (format "load: symbol=%s load-file=%s" require-symbol load-file)
     orig-fun args)))

(when abn-enable-timed-loads-p
  ;; (advice-add 'package-initialize
  ;;             :around #'abn//debug-time-package-initialize)
  (advice-add 'require
              :around #'abn//debug-require-timer)
  ;; (advice-add 'load
  ;;             :around #'abn//debug-time-load)
  )

(provide 'abn-module-debug)
;;; abn-module-debug.el ends here
