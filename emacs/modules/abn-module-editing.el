;;; abn-module-editing.el --- Setup editing

;;; Commentary:
;;

;;; Code:

;; Formatting
(use-package abn-funcs-editing
  :ensure nil ; local package
  :general
  (abn/define-leader-keys
   "b!" 'abn/shell-command-on-buffer
   "jo" 'open-line
   "j=" 'abn/indent-region-or-buffer
   "jS" 'abn/split-and-new-line
   "jk" 'abn/evil-goto-next-line-and-indent
   "tl" 'toggle-truncate-lines
   "yf" 'abn/yank-tilde-file-path)
  (:states '(normal visual operator motion)
   "gm" 'abn/goto-middle-of-line))

(use-package flycheck
  :defer t)

;; Inserts dummy text.
(use-package lorem-ipsum
  :defer t
  :general
  (abn/define-leader-keys
   "ill" 'lorem-ipsum-insert-list
   "ilp" 'lorem-ipsum-insert-paragraphs
   "ils" 'lorem-ipsum-insert-sentences)
  :init
  (abn-declare-prefix "il" "lorem ipsum"))

;; Moves the current line or region up or down.
(use-package move-text
  :defer t
  :general
  (:states '(normal)
   "[ e" 'move-text-up
   "] e" 'move-text-down))

;; Unfills paragraphs.
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

(use-package uniquify
  :defer 2
  :ensure nil ; built-in package
  :config
  ;; When having windows with repeated filenames, uniquify them
  ;; by folder rather than the default suffix <2>, <3>, ..., <n>.
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
	;; Ignore special buffers.
	uniquify-ignore-buffers-re "^\\*"))

(use-package wgrep
  :defer t)

(provide 'abn-module-editing)
;;; abn-module-editing.el ends here