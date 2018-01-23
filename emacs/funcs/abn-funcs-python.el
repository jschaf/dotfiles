;;; abn-funcs-python.el --- Functions for python

;;; Commentary:
;;

;;; Code:

(defun abn/inihibit-electric-indent-mode ()
  "Prevent re-indent for python mode.
See https://emacs.stackexchange.com/questions/13557."
  (setq-local electric-indent-inhibit t))

(provide 'abn-funcs-python)
;;; abn-funcs-python.el ends here
