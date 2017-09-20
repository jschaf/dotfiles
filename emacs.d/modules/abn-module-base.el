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

(use-package autorevert
  :defer 1
  :ensure nil ; built-in package
  :config
  ;; Reverts buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode t))

(provide 'abn-module-base)
;;; abn-module-base.el ends here
