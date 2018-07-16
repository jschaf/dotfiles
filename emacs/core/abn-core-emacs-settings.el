;;; abn-core-emacs-settings.el --- Editor defaults

;;; Commentary:
;;

;;; Code:

;; Alphabetical order by identifier.

(defun abn//make-cache-dir (dir)
  "Create DIR in `abn-cache-dir', making parents and returning DIR."
  (let ((new-dir (concat abn-cache-dir "/" dir)))
    (make-directory new-dir 'parents)
    (file-truename new-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,(abn//make-cache-dir "auto-save-list") t)))

;; Directory to store backup files.
(setq-default backup-directory-alist `(("." . ,(abn//make-cache-dir "backups"))))
(setq-default tramp-auto-save-directory (abn//make-cache-dir "tramp-auto-save"))
(setq-default tramp-backup-directory-alist backup-directory-alist)
(setq-default url-cache-directory (abn//make-cache-dir "url"))
(setq-default url-configuration-directory url-cache-directory)
(setq-default savehist-file (concat abn-cache-dir "/" "history"))

;; Silence ad-handle-definition about advised functions getting redefined.
(setq ad-redefinition-action 'accept)

;; Displays column number in the mode line.
(setq column-number-mode t)

;; Moves the custom file out of the bottom of init.el.
(setq custom-file "~/.emacs.d/custom.el")

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; We don't share the file-system with anyone else.
(setq create-lockfiles nil)

;; Never insert tabs.
(setq-default indent-tabs-mode nil)

;; Skip startup screen.
(setq inhibit-startup-screen t)

;; Start with a blank canvas.
(setq initial-scratch-message "")

;; Warns when opening files bigger than 10MB.
(setq large-file-warning-threshold (* 10 1024 1024))

;; Load the newer .elc or .el file, rather than stopping at .elc.
(setq load-prefer-newer t)

(setq mouse-yank-at-point t)

;; Potentially speed up cursor operations
;; https://emacs.stackexchange.com/questions/28736
(setq auto-window-vscroll nil)

;; Too useful to disable
(put 'narrow-to-region 'disabled nil)

;; Newline at end of file.
(setq require-final-newline t)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Store pastes from other programs in the kill-ring before
;; overwriting with Emacs' yanks.
(setq save-interprogram-paste-before-kill t)

;; Double space for sentences.
(setq-default sentence-end-double-space t)

;; Enables nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Number backup files.
(setq version-control t)

;; Don't ask for confirmation when opening symlinked file.
(setq vc-follow-symlinks t)

;; Make backup files even when in version controlled directory.
(setq vc-make-backup-files t)

;; y is shorter than yes.
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'abn-core-emacs-settings)
;;; abn-core-emacs-settings.el ends here
