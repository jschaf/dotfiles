;;; abn-core-emacs-settings.el --- Editor defaults

;;; Commentary:
;;

;;; Code:

;; Alphabetical order by identifier.

;; Transforms backup file names.
(defvar abn-auto-save-directory (concat abn-cache-dir "/auto-save-list"))
(mkdir abn-auto-save-directory 'parents)
(setq auto-save-file-name-transforms
      `((".*" ,abn-auto-save-directory t)))

;; Directory to store backup files.
(defvar abn-backup-directory (concat abn-cache-dir "/backups"))
(mkdir abn-backup-directory 'parents)
(setq backup-directory-alist `(("." . ,abn-backup-directory)))

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
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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

;; Too useful to disable
(put 'narrow-to-region 'disabled nil)

;; Newline at end of file.
(setq require-final-newline t)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Store pastes from other programs in the kill-ring before
;; overwriting with Emacs' yanks.
(setq save-interprogram-paste-before-kill t)

(setq savehist-file (concat abn-cache-dir "/history"))

;; Double space for sentences.
(setq-default sentence-end-double-space t)

;; Enables nice scrolling.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Move cache outside of version control.
(setq url-cache-directory (concat abn-cache-dir "/url"))

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
