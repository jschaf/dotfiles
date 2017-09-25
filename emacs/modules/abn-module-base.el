;;; abn-module-base.el --- Important packages.

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-base
  :defer t
  :ensure nil ; local package
  :commands
  (abn/set-gc-cons-threshold-to-2mb
   abn/set-gc-cons-threshold-to-50mb
   abn/start-server-if-not-running
   abn/stop-watch
   abn/system-is-mac
   abn/system-is-linux
   abn/system-is-mswindows))

(use-package auto-compile
  :defer 2
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package autorevert
  :defer 1
  :diminish auto-revert-mode
  :ensure nil ; built-in package
  :config
  ;; Reverts buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode t))

(use-package deferred
  :defer t)

(use-package esup
  :defer t
  :ensure nil ; development package
  :commands (esup)
  :load-path "~/prog/esup")

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

(use-package server
  :defer 1
  :ensure nil ; built-in packages
  :config
  ;; Start server immediately.
  (abn/start-server-if-not-running)
  ;; Ensure it stays running.
  (run-with-idle-timer
   10 ; seconds
   'repeat ; Repeat after idle for 10 seconds.
   #'abn/start-server-if-not-running))

;; Set GC high for minibuffer which usually involves expensive fuzzy
;; matching.
(add-hook 'minibuffer-setup-hook #'abn/set-gc-cons-threshold-to-50mb)

;; Reset low afterward to ensure emacs GC pauses aren't perceptible.
(add-hook 'minibuffer-exit-hook #'abn/set-gc-cons-threshold-to-2mb)

(provide 'abn-module-base)
;;; abn-module-base.el ends here
