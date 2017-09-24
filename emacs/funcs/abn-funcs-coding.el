;;; abn-funcs-coding.el --- Functions for coding

;;; Commentary:
;;

;;; Code:

(defun abn/pick-zeal-docset ()
  "Choose custom docsets based on the buffer."
  (setq zeal-at-point-docset
	(cond
	 ((equal major-mode 'js2-mode) "goog,javascript,angularjs")
	 (t nil))))

(provide 'abn-funcs-coding)
;;; abn-funcs-coding.el ends here
