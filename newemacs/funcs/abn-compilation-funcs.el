;;; abn-compilation-funcs.el --- Functions for compilation

;;; Commentary:
;;


(defun abn/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(provide 'abn-compilation-funcs)
;;; abn-compilation-funcs.el ends here
