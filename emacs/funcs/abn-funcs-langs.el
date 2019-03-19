;;; abn-funcs-langs.el --- Functions for langs

;;; Commentary:
;;

;;; Code:

(defun abn/go-mode-hook-init-format-on-save ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2)
  (setq indent-tabs-mode 1))

(defvar abn//zsh-files-with-strange-names '("prompt_pure_setup")
  "Files that should use sh-mode[zsh] instead of sh-mode[default].")

(defun abn//switch-to-zsh-mode-by-file-name ()
  "Switch to ZSH for specially named files."
  (dolist (name abn//zsh-files-with-strange-names)
    (when (string-match name buffer-file-name)
      (message "Switch to ZSH because file name matched name %s" name)
      (sh-set-shell "zsh"))))

(provide 'abn-funcs-langs)
;;; abn-funcs-langs.el ends here
