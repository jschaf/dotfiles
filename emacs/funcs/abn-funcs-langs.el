;;; abn-funcs-langs.el --- Functions for langs

;;; Commentary:
;;

;;; Code:

(defun abn/go-mode-hook-init-format-on-save ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2)
  (setq indent-tabs-mode 1))

(provide 'abn-funcs-langs)
;;; abn-funcs-langs.el ends here
