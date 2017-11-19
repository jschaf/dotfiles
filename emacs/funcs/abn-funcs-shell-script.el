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
                  "\n"
                  (format "%s \"$@\"" name)
                  ))
  (forward-line -3)
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
  (find-file (concat "~/.dotfiles-work/zsh/work-functions/" name))
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

;; The following functions were used to migrate ZSH autoloads to
;; explicitly call themselves instead of just defining themselves.
;; Meaning:
;;
;; function foo() {}
;;
;; Gets turned into this:
;;
;; function foo() {}
;; foo "$@"
;;
;; The reason is because it broke on my work computer.
;;
;; The actual transformation code looked like:
;;
;; (with-current-buffer "zsh<.dotfiles-work>"
;;   (loop for path in (dired-get-marked-files)
;;         do (abn//add-invocation-to-zsh-autoload-file path)))

(defun abn//extract-zsh-function-name-from-autoload (&optional buffer)
  "Gets the first ZSH function name from a file."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (search-forward-regexp "function \\([a-zA-Z1-9_-]+\\)" (point-max) 'noerror)
      (match-string-no-properties 1))))

(defun abn//zsh-file-invokes-itself (&optional buffer)
  "Checks if the current buffer calls itself."
  (interactive)
  (unless buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward-regexp (format "^%s\\( \"$@\"\\)?"
                                   (file-name-nondirectory (buffer-file-name)))
                           (point-max)
                           'no-error)))

(defun abn//add-invocation-to-zsh-autoload-file (path)
  "Adds the invocation like <function-name> \"$@\" to an autoload."
  (with-current-buffer (find-file-noselect path)
    (goto-char (point-min))
    (unless (abn//zsh-file-invokes-itself)
      (let ((func-name (file-name-nondirectory (buffer-file-name))))
        (goto-char (point-max))
        (insert (format "%s \"$@\"\n" func-name))
        (save-buffer)))))

(provide 'abn-funcs-shell-script)
;;; abn-funcs-shell-script.el ends here
