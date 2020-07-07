;;; abn-module-autocomplete.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-autocomplete
  :defer t
  :ensure nil ; local package
  :commands
  (abn/disable-eldoc-mode
   abn/else-kill-always
   abn/else-next-if-nearby
   abn/add-else-template-mappings
   abn/insert-last-tmux-command-output)
  :bind
  (:map abn-leader-map
   ("it" . abn/insert-last-tmux-command-output)))

(use-package company
  :defer 1
  :ensure t
  :diminish company-mode
  :bind
  (:map company-mode-map
   ("M-/" . company-complete))
  (:map company-active-map
   ("M-n" . company-select-next)
   ("M-p" . company-select-previous)
   ("C-j" . company-select-next)
   ("C-k" . company-select-previous)
   ("TAB" . company-complete-selection))
  :init
  (global-company-mode)
  :config
  (setq tab-always-indent 'complete))

(use-package company-ycmd
  :defer 2
  :ensure t
  :init
  :config
  (add-to-list 'company-backends 'company-ycmd))

(use-package else-mode
  :disabled
  :defer t
  :ensure nil ; development package
  :load-path "~/prog/else"
  :diminish else-mode
  :commands (else-mode)
  :bind
  (:map else-mode-key-map
   ("M-n" . else-next)
   ("M-p" . else-previous)
   ("M-N" . else-expand)
   ("M-P" . abn/else-kill-always))

  :init
  (defvar abn-else-template-dir (expand-file-name abn-dir "else-templates"))
  (add-to-list 'load-path abn-else-template-dir)
  ;; Language hooks
  (add-hook 'emacs-lisp-mode-hook #'else-mode)
  (add-hook 'sh-mode-hook #'else-mode)
  (setq else-kill-proceed-to-next-placeholder t)

  :config
  (abn/add-else-template-mappings
   "Elisp" "Emacs-Lisp"
   "ELSE-Template" "Template"
   "Shell-script" "Shell")

  (define-key else-menu-mode-map (kbd "C-j") 'popup-next)
  (define-key else-menu-mode-map (kbd "C-k") 'popup-previous)
  (define-key else-menu-mode-map (kbd "M-n") 'popup-next)
  (define-key else-menu-mode-map (kbd "M-p") 'popup-previous))

(use-package else-template-mode
  :defer t
  :ensure nil ; development package
  :load-path "~/prog/ELSE"
  :mode ("\\.lse\\'" . else-template-mode)
  :commands (else-template-mode else-template-compile-buffer)
  :config
  (evil-define-key '(normal insert) "C-c C-c" 'else-template-compile-buffer))

(use-package flycheck
  :defer 3
  :ensure t
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config)

(use-package flycheck-ycmd
  :defer 3
  :ensure t
  :init
  (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
  :config)

(use-package hippie-exp
  :defer t
  :ensure nil ; built-in package
  :init
  (global-set-key (kbd "M-/") 'hippie-expand)
  ;; Disables "Using try-expand-dabbrev" on completions.
  (setq hippie-expand-verbose nil)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current
          ;; buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other
          ;; buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill
          ;; ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters
          ;; as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev
          ;; tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the
          ;; buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the
          ;; buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many
          ;; characters as unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))) 

(use-package popup
  :defer t)

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands (yas-hippie-try-expand)
  :init
  (setq yas-verbosity 1)
  (setq yas-snippet-dirs '((expand-file-name abn-dir "emacs/snippets")))
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; Disable default yas minor mode map and use hippie integration.
  (setq yas-minor-mode-map (make-sparse-keymap))
  (setq yas-wrap-around-region t)
  :config
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(use-package ycmd
  :defer 2
  :ensure t
  :init
  :config)

(provide 'abn-module-autocomplete)
;;; abn-module-autocomplete.el ends here
