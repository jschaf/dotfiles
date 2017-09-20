;;; abn-module-evil.el --- Basic evil-mode configuration.

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'evil)

;; Evil Plugin Packages
(use-package abn-funcs-evil
  :defer t
  :ensure nil ; local package
  :general
  (:states '(normal visual motion)
   "J" 'abn/evil-next-visual-line-5
   "K" 'abn/evil-previous-visual-line-5)
  (:states 'visual
   ">" 'abn/shift-right-visual
   "<" 'abn/shift-left-visual))

(use-package evil
  :demand ;; TODO: can we lazy load evil?
  :config
  (evil-mode 1)
  (require 'abn-local-evil-config))

;; Enables two char keypress to exit most modes.
(use-package evil-escape
  :defer t
  :diminish evil-escape-mode
  :commands (evil-escape-pre-command-hook)
  :init
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  :config
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t))

;; Edit multiple regions with the same content simultaneously.
(use-package evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :general
  (abn/define-leader-keys
   "se" 'evil-iedit-state/iedit-mode)
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil))

(use-package evil-nerd-commenter
  :defer t
  :general
  (abn/define-leader-keys
   ";"  'evilnc-comment-operator))

;; Enables vim style numeric incrementing and decrementing.
(use-package evil-numbers
  :defer t
  :general
  (abn/define-leader-keys
   "n+" 'evil-numbers/inc-at-pt
   "n=" 'evil-numbers/inc-at-pt
   "n-" 'evil-numbers/dec-at-pt))

;; Replace text with the contents of a register.
(use-package evil-replace-with-register
  :defer t
  :general
  (:states '(motion)
   "gr" 'evil-replace-with-register))

;; Emulates the vim surround plugin.
(use-package evil-surround
  :defer t
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
  (:states 'visual
   "S" 'evil-surround-region
   "gS" 'evil-Surround-region))

;; Change the cursor display in a terminal emacs.
(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :demand
  :init
  (setq evil-visual-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor 'hbar)
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-unimpaired
  :defer t
  :ensure nil ; Local package
  :general
  (:states 'normal
   ;; From tpope's unimpaired.
   "[ SPC" 'evil-unimpaired/insert-space-above
   "] SPC" 'evil-unimpaired/insert-space-below
   "[ e" 'move-text-up
   "] e" 'move-text-down
   "[ e" ":move'<--1"
   "] e" ":move'>+1"
   "[ e" 'move-text-up
   "] e" 'move-text-down
   "[ b" 'previous-buffer
   "] b" 'next-buffer
   "[ f" 'evil-unimpaired/previous-file
   "] f" 'evil-unimpaired/next-file
   "] l" 'abn/next-error
   "[ l" 'abn/previous-error
   "] q" 'abn/next-error
   "[ q" 'abn/previous-error
   "[ t" 'evil-unimpaired/previous-frame
   "] t" 'evil-unimpaired/next-frame
   "[ w" 'previous-multiframe-window
   "] w" 'next-multiframe-window
   ;; Selects pasted text.
   "g p" (kbd "` [ v ` ]")
   ;; Pastes above or below with newline.
   "[ p" 'evil-unimpaired/paste-above
   "] p" 'evil-unimpaired/paste-below))

;; Starts a * or # search from the visual selection.
(use-package evil-visualstar
  :defer t
  :general
  (:states 'visual
   "*" 'evil-visualstar/begin-search-forward
   "#" 'evil-visualstar/begin-search-backward))

;; Shows number of matches in mode-line when searching with evil.
(use-package evil-anzu
  ;; Lazy loading doesn't make a much sense because evil-anzu
  ;; only defines four defadvices for `evil-search' `evil-ex'
  ;; `evil-flash' `evil-ex'
  :demand)

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode)

;; Evil keybindings for magit.
(use-package magit
  :defer t
  :config
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
    "K" 'magit-discard
    "L" 'magit-log-popup)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    "K" 'magit-discard
    "l" 'magit-log-popup
    "h" 'magit-diff-toggle-refine-hunk))

(use-package org
  :defer t
  :ensure nil ;; We only want to add config options, not require org.
  :config
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line
    "L" 'org-end-of-line
    "t" 'org-todo
    ",c" 'org-cycle
    "TAB" 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft
    ">" 'org-metaright))

(provide 'abn-module-evil)
;;; abn-module-evil.el ends here
