;;; abn-funcs-base.el --- Functions for base

;;; Commentary:
;;

;;; Code:

(defmacro abn/stop-watch (&rest forms)
  (let ((temp-var (make-symbol "start")))
    `(let* ((,temp-var (float-time)))
       (progn . ,forms)
       (- (float-time) ,temp-var))))

(defun abn/set-gc-cons-threshold-to-2mb ()
  (setq gc-cons-threshold (* 2 1000 1000)))

(defun abn/set-gc-cons-threshold-to-50mb ()
  (setq gc-cons-threshold (* 50 1000 1000)))

(defun abn/start-server-if-not-running ()
  "Start the server if it's not running."
  (unless (server-running-p) (server-start)))

(defun abn/system-is-mac ()
  (eq system-type 'darwin))

(defun abn/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun abn/system-is-mswindows ()
  (eq system-type 'windows-nt))

(provide 'abn-funcs-base)
;;; abn-funcs-base.el ends here
