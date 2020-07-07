;;; abn-module-evil.el --- Basic evil-mode configuration.

;;; Code:
(eval-when-compile
  (require 'cl))

;; Evil Plugin Packages
(use-package abn-funcs-evil
  :defer t
  :ensure nil ; local package
  :commands
  (abn/evil-next-visual-line-5
   abn/evil-previous-visual-line-5
   abn/shift-right-visual
   abn/shift-left-visual))

(use-package evil
  :demand
  :init
  ;; Must be set before evil is loaded.
  (setq evil-respect-visual-line-mode t)
  :config
  ;; Be really evil.
  (evil-mode 1)

  ;; Set SPACE to invoke `abn-leader-map' in modes except emacs and insert.
  (evil-define-key '(normal visual motion) 'global
    (kbd abn-leader-key) abn-leader-map)

  ;; Set the M-m keybinding for `abn-leader-map' in all modes.
  (evil-define-key '(normal insert visual motion emacs) 'global
    (kbd abn-emacs-leader-key) abn-leader-map)

  ;; Prevents esc-key from translating to meta-key in terminal mode.
  (setq evil-esc-delay 0)

  ;; It's better that the default value is too small than too big.
  (setq-default evil-shift-width 2)

  ;; * and # search using symbols.
  (setq-default evil-symbol-word-search t)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect.
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  ;; Controls position of the mode line tag for the current mode,
  ;; e.g. <N>, <I>, etc.  Before places it before the major-mode.
  (setq evil-mode-line-format 'before)


  ;; Cursor colors.
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
  (setq evil-emacs-state-cursor '("SkyBlue2" box))
  (setq evil-hybrid-state-cursor '("SkyBlue2" (bar . 2)))
  (setq evil-replace-state-cursor '("chocolate" (hbar . 2)))
  (setq evil-evilified-state-cursor '("LightGoldenrod3" box))
  (setq evil-visual-state-cursor '("gray" (hbar . 2)))
  (setq evil-motion-state-cursor '("plum3" box))
  (setq evil-lisp-state-cursor '("HotPink1" box))
  (setq evil-iedit-state-cursor '("firebrick1" box))
  (setq evil-iedit-state-cursor-insert '("firebrick1" (bar . 2)))

  ;; http://emacs.stackexchange.com/questions/14940
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Major modes that should default to an insert state.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  ;; Sets more useful movement commands.
  (evil-define-key '(normal motion visual) 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    ;; H is set in abn-module-editing to `abn/back-to-indentation-or-beginning'
    (kbd "J") 'abn/evil-next-visual-line-5
    (kbd "K") 'abn/evil-previous-visual-line-5
    (kbd "gj") 'evil-join
    (kbd "gh") 'evil-window-top
    (kbd "gl") 'evil-window-bottom
    (kbd "L") 'evil-end-of-line
    (kbd "C-j") 'scroll-up-command
    (kbd "C-k") 'scroll-down-command)

  (abn/define-leader-keys
    "jt" 'evil-window-top
    "jb" 'evil-window-bottom))

;; Shows number of matches in mode-line when searching with evil.
(use-package evil-anzu
  ;; Lazy loading doesn't make a much sense because evil-anzu
  ;; only defines four defadvices for `evil-search' `evil-ex'
  ;; `evil-flash' `evil-ex'
  :demand)

;; Motions and text objects for delimited arguments, e.g. the params
;; in `def func(foo, bar, baz)'.
(use-package evil-args
  :defer t
  :bind
  (:map evil-inner-text-objects-map
   ("a" . evil-inner-arg)
   :map evil-outer-text-objects-map
   ("a" . evil-outer-arg)))

;; Enables two char keypress to exit most modes.
(use-package evil-escape
  :defer t
  :diminish evil-escape-mode
  :commands (evil-escape-pre-command-hook)
  :init
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t))

;; Easy text exchange operator
(use-package evil-exchange
  :defer t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion
      "gx" 'evil-exchange
      "gX" 'evil-exchange-cancel)))

;; Highlight changes with evil operations
(use-package evil-goggles
  :defer 3
  :after (evil)
  :diminish evil-goggles-mode
  :config
  (setq evil-goggles-duration 0.060)
  (evil-goggles-mode))

;; Edit multiple regions with the same content simultaneously.
(use-package evil-iedit-state
  :defer t
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :bind
  (:map abn-leader-map
   ("se" . evil-iedit-state/iedit-mode))
  :init
  (setq iedit-current-symbol-default t
        iedit-only-at-symbol-boundaries t
        iedit-toggle-key-default nil))

(use-package evil-nerd-commenter
  :defer t
  :bind
  (:map abn-leader-map
   (";"  . evilnc-comment-operator)))

;; Enables vim style numeric incrementing and decrementing.
(use-package evil-numbers
  :defer t
  :bind
  (:map abn-leader-map
   ("n+" . evil-numbers/inc-at-pt)
   ("n=" . evil-numbers/inc-at-pt)
   ("n-" . evil-numbers/dec-at-pt)))

;; Replace text with the contents of a register.
(use-package evil-replace-with-register
  :defer t
  :bind
  (:map evil-motion-state-map
   ("gr" . evil-replace-with-register)))

;; Emulates the vim surround plugin.
(use-package evil-surround
  :defer t
  :bind
  (:map evil-operator-state-map
   ("s" . evil-surround-edit)
   ("S" . evil-Surround-edit)
   :map evil-visual-state-map
   ("S" . evil-surround-region)
   ("gS" . evil-Surround-region))
  :config
  (setq-default evil-surround-pairs-alist
                ;; Add \ to mean escaped string.
                (cons '(?\\ . ("\\\"" . "\\\""))
                      evil-surround-pairs-alist)))

(use-package evil-unimpaired
  :defer t
  :ensure nil ; Local package
  :bind
  (:map evil-normal-state-map
   ;; From tpope's unimpaired.
   ("[ SPC" . evil-unimpaired/insert-space-above)
   ("] SPC" . evil-unimpaired/insert-space-below)
   ("[ b" . previous-buffer)
   ("] b" . next-buffer)
   ("[ f" . evil-unimpaired/previous-file)
   ("] f" . evil-unimpaired/next-file)
   ("] l" . abn/next-error)
   ("[ l" . abn/previous-error)
   ("] q" . abn/next-error)
   ("[ q" . abn/previous-error)
   ("[ t" . evil-unimpaired/previous-frame)
   ("] t" . evil-unimpaired/next-frame)
   ("[ w" . previous-multiframe-window)
   ("] w" . next-multiframe-window)
   ;; Selects pasted text.
   ;; "g p" (kbd "` [ v ` ]")
   ;; Pastes above or below with newline.
   ("[ p" . evil-unimpaired/paste-above)
   ("] p" . evil-unimpaired/paste-below)))

;; Starts a * or # search from the visual selection.
(use-package evil-visualstar
  :defer t
  :bind
  (:map evil-visual-state-map
   ("*" . evil-visualstar/begin-search-forward)
   ("#" . evil-visualstar/begin-search-backward)))

;; Evil keybindings for magit.
(use-package magit
  :defer t
  :config
  (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
  (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
    "K" 'magit-discard
    "L" 'magit-log)
  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    "K" 'magit-discard
    "L" 'magit-log
    "h" 'magit-diff-toggle-refine-hunk))

(use-package org
  :defer t
  :ensure nil ;; We only want to add config options, not require org.
  :config
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    ;; This is too burned in as join-line to be useful.
    ;; "gj" 'outline-next-visible-heading
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
    ))

(provide 'abn-module-evil)
;;; abn-module-evil.el ends here
