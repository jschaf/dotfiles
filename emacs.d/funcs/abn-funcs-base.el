;;; abn-funcs-base.el --- Functions for base

;;; Commentary:
;;

;;; Code:

(defun abn/set-gc-cons-threshold-to-2mb ()
  (setq gc-cons-threshold (* 2 1000 1000)))

(defun abn/set-gc-cons-threshold-to-50mb ()
  (setq gc-cons-threshold (* 50 1000 1000)))

(defun abn/start-server-if-not-running ()
  "Start the server if it's not running."
  (unless (server-running-p) (server-start)))

(provide 'abn-funcs-base)
;;; abn-funcs-base.el ends here
