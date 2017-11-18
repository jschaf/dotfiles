;;; abn-funcs-python.el --- Functions for python

;;; Commentary:
;;

;;; Code:

(defun abn/remove-colon-from-electric-indent-chars ()
  "Prevent ':' from invoking electric indent.
Used to prevent `python-mode' from reindenting the current line
when typing ':'.  See:
https://emacs.stackexchange.com/questions/3322/python-auto-indent-problem"
  (setq-local electric-indent-chars (delq ?: electric-indent-chars)))

(provide 'abn-funcs-python)
;;; abn-funcs-python.el ends here
