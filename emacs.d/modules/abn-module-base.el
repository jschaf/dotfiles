;;; abn-module-base.el --- Config for base

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-emacs-config
  :defer t
  :ensure nil ; local package
  )

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  ;; Enable eldoc in `eval-expression'.
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  ;; Enable eldoc in IELM.
  (add-hook 'ielm-mode-hook #'eldoc-mode))

(use-package recentf
  :defer 1
  :init
  (setq-default recentf-save-file (concat abn-cache-dir "/recentf")))

;; Save point position between sessions.
(use-package saveplace
  :defer 1
  :config
  (save-place-mode)
  (setq save-place-file (concat abn-cache-dir "/places")))


;; Variables

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Number backup files.
(setq version-control t)

;; Make backup files even when in version controlled directory.
(setq vc-make-backup-files t)

;; Directory to store backup files.
(defvar abn-backup-directory (concat abn-cache-dir "/backups"))
(mkdir abn-backup-directory 'parents)
(setq backup-directory-alist `(("." . ,abn-backup-directory)))

;; Transforms backup file names.
(defvar abn-auto-save-directory (concat abn-cache-dir "/auto-save-list"))
(mkdir abn-auto-save-directory 'parents)
(setq auto-save-file-name-transforms
      `((".*" ,abn-auto-save-directory t)))

;; Move cache outside of version control.
(setq url-cache-directory (concat abn-cache-dir "/url"))

(provide 'abn-module-base)
;;; abn-module-base.el ends here
