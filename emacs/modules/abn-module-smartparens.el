;;; abn-module-smartparens.el --- Smartparens module

;;; Commentary:
;;

;;; Code:

(use-package abn-funcs-smartparens
  :defer t
  :ensure nil ; local package
  :commands
  (abn//conditionally-enable-smartparens-mode
   abn/smartparens-pair-newline
   abn/smartparens-pair-newline-and-indent))

(use-package smartparens
  :demand
  :diminish smartparens-mode
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :general
  (abn/define-leader-keys
   "js" 'sp-split-sexp
   "jn" 'sp-newline)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0.2
	;; fix paren highlighting in normal mode
	sp-show-pair-from-inside t
	sp-cancel-autoskip-on-backward-movement nil
	sp-highlight-pair-overlay nil
	sp-highlight-wrap-overlay nil
	sp-highlight-wrap-tag-overlay nil)

  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'comint-mode-hook 'smartparens-mode)

  ;; Enables smartparens-mode in `eval-expression'.
  (add-hook 'minibuffer-setup-hook
	    'abn//conditionally-enable-smartparens-mode)

  (show-smartparens-global-mode +1)
  ;; Don't create a pair with single quote in minibuffer.
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-pair "{" nil :post-handlers
	   '(:add (abn/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
	   '(:add (abn/smartparens-pair-newline-and-indent "RET"))))

(provide 'abn-module-smartparens)

;;; abn-module-smartparens.el ends here
