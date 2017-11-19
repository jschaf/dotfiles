;;; abn-module-autocomplete.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-autocomplete
  :defer t
  :ensure nil ; local package
  :commands (abn/disable-eldoc-mode))

(use-package else-mode
  :defer t
  :ensure nil ; development package
  :load-path "~/prog/ELSE"
  :commands (else-mode)
  :general
  (:keymaps
   'else-mode-key-map
   "M-n" 'else-next
   "M-p" 'else-previous
   "M-N" 'else-expand
   "M-P" 'else-kill
   :keymaps
   'else-menu-mode-map
   "C-j" 'popup-next
   "C-k" 'popup-previous
   )
  :init
  (defvar abn-else-template-dir "~/.dotfiles/emacs/else-templates")
  (add-to-list 'load-path abn-else-template-dir)
  (evil-define-minor-mode-key 'insert 'else-mode
    (kbd "M-e") 'else-expand)

  ;; Language hooks
  (add-hook 'emacs-lisp-mode-hook #'else-mode)
  )

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

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands (yas-hippie-try-expand)
  :init
  (setq yas-verbosity 1)
  (setq yas-snippet-dirs '("~/.dotfiles/emacs/snippets"))
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; Disable default yas minor mode map and use hippie integration.
  (setq yas-minor-mode-map (make-sparse-keymap))
  (setq yas-wrap-around-region t)
  :config
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(provide 'abn-module-autocomplete)
;;; abn-module-autocomplete.el ends here
