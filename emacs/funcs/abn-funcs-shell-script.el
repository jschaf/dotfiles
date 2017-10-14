;;; abn-funcs-shell-script.el --- Functions for shell-script

;;; Commentary:
;;

;;; Code:

(defun abn//insert-zsh-function (name)
  "Inserts the skeleton for a ZSH function."
  (insert (concat "#!/bin/zsh\n"
                  "\n"
                  (format "function %s() {\n" name)
                  "  \n"
                  "}\n"
                  ))
  (forward-line -2)
  (goto-char (line-end-position))
  (save-buffer)
  (shell-command (format "chmod +x %s" (buffer-file-name)))
  (shell-script-mode))

(defun abn/new-zsh-function (name)
  "Creates a new ZSH function of NAME."
  (interactive (list (read-string "ZSH function name: ")))
  (find-file (concat "~/.dotfiles/zsh/functions/" name))
  (abn//insert-zsh-function name))

(defun abn/new-zsh-function-iosource (name)
  "Creates a new ZSH function of NAME."
  (interactive (list (read-string "ZSH iosource function name: ")))
  (find-file (concat "~/.dotfiles/zsh/iosource/" name))
  (abn//insert-zsh-function name))

(defun abn/new-zsh-function-work (name)
  "Creates a new ZSH function of NAME."
  (interactive (list (read-string "ZSH work-function name: ")))
  (find-file (concat "~/.dotfiles-work/zsh/work/" name))
  (abn//insert-zsh-function name))

(defun abn/new-zsh-function-host (name)
  "Creates a new ZSH function of NAME for the current host."
  (interactive (list (read-string "ZSH host-function name: ")))
  (let ((host-dir (concat (expand-file-name "~/.dotfiles-work/")
                          "host-"
                          (system-name))))
    (if (file-exists-p host-dir)
        (progn
          (find-file (concat host-dir "/zsh/host/" name))
          (abn//insert-zsh-function name))
      (message "Directory does not exists: %s" host-dir))))

(defun abn/new-zsh-key-widget (name)
  "Creates a new ZSH function of NAME."
  (interactive (list (read-string "ZSH widget name: ")))
  (find-file (concat "~/.dotfiles/zsh/widgets/" name))
  (abn//insert-zsh-function)
  (goto-char (point-min))
  (forward-line 1)
  (insert (format "\nfunction __%s() {\n  \n}\n" name))
  (goto-char (point-max))
  (insert (format "\n%s\n" name))
  (forward-line -4)
  (goto-char (line-end-position))
  (save-buffer))

(provide 'abn-funcs-shell-script)
;;; abn-funcs-shell-script.el ends here
