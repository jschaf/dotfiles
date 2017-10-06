;;; abn-funcs-java.el --- Functions for java

;;; Commentary:
;;

;;; Code:

(defun abn/set-column-limit-to-100 ()
  "Sets the column limit to 100."
  ;; We run this in `c-mode-common-hook' because `java-mode' lives
  ;; inside `cc-mode' which doesn't define `java-mode-hook' until
  ;; runtime.
  (when (string-equal major-mode "java-mode")
    (setq-local fill-column 100)))

(defun abn/set-java-indent-to-2-spaces ()
  (when (string-equal major-mode "java-mode")
    (setq-local c-basic-offset 2)))

(provide 'abn-funcs-java)
;;; abn-funcs-java.el ends here
