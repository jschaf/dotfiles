;;; abn-module-editing.el --- Setup editing

;;; Commentary:
;;

;;; Code:

;; Packages are in alphabetical order.
(use-package lorem-ipsum
  :defer t
  :init
  (abn-declare-prefix "il" "lorem ipsum")
  (abn-define-leader-keys
    "ill" 'lorem-ipsum-insert-list
    "ilp" 'lorem-ipsum-insert-paragraphs
    "ils" 'lorem-ipsum-insert-sentences))

(use-package uniquify
  :defer t
  :ensure nil ; built-in package
  :config
  ;; When having windows with repeated filenames, uniquify them
  ;; by folder rather than the default suffix <2>, <3>, ..., <n>.
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
	;; Ignore special buffers.
	uniquify-ignore-buffers-re "^\\*"))


;; Variables

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; y is shorter than yes.
(fset 'yes-or-no-p 'y-or-n-p)

;; Reverts buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

(add-to-list 'hippie-expand-try-functions-list
	     'try-complete-lisp-symbol-partially)

;; Don't ask for confirmation when opening symlinked file.
(setq vc-follow-symlinks t)

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; Warns when opening files bigger than 100MB.
(setq large-file-warning-threshold (* 10 1000 1000))

;; Double space for sentences.
(setq-default sentence-end-double-space t)

;; Newline at end of file.
(setq require-final-newline t)

(provide 'abn-module-editing)
;;; abn-module-editing.el ends here
