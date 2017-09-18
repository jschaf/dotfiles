;;; abn-module-base.el --- Config for base

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-emacs-config
  :ensure nil ; local package
  :defer t
  )

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :config
  (progn
    ;; Enable eldoc in `eval-expression'.
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; Enable eldoc in IELM.
    (add-hook 'ielm-mode-hook #'eldoc-mode)))

(use-package saveplace
  :init
  (when (fboundp 'save-place-mode)
    (save-place-mode))
  ;; Save point position between sessions
  (setq save-place-file (concat abn-cache-dir "/places")))

(provide 'abn-module-base)
;;; abn-module-base.el ends here
