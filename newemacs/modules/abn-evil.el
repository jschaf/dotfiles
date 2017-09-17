;;; abn-evil.el --- Basic evil-mode configuration.

;;; Code:

(require 'cl)
(require 'evil)

;;; goto-chg lets you use the g-; and g-, to go to recent changes
(abn-require-packages
 '(goto-chg))


;; Evil Plugin Packages

;; Enables two char keypress to exit most modes.
(use-package evil-escape
  :diminish evil-escape-mode
  :commands (evil-escape-pre-command-hook)
  :init
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  :config
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t))

(use-package evil-nerd-commenter
  :commands evilnc-comment-operator
  :init
  (abn-define-leader-keys
   ";"  'evilnc-comment-operator))

;; Enables vim style numeric incrementing and decrementing.
(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (abn-define-leader-keys
   "n+" 'evil-numbers/inc-at-pt
   "n=" 'evil-numbers/inc-at-pt
   "n-" 'evil-numbers/dec-at-pt))

;; Emulates the vim surround plugin.
(use-package evil-surround
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))

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
  :ensure nil ; Local package
  :general
  (:states '(normal)
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
           "] l" 'spacemacs/next-error
           "[ l" 'spacemacs/previous-error
           "] q" 'spacemacs/next-error
           "[ q" 'spacemacs/previous-error
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
  :general
  (:states '(visual)
           "*" 'evil-visualstar/begin-search-forward
           "#" 'evil-visualstar/begin-search-backward))

;; Shows number of matches in mode-line when searching with evil.
(use-package evil-anzu)


;;; Evil options
(evil-mode 1)

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

;; evil-want-Y-yank-to-eol must be set via customize to have an effect.
(customize-set-variable 'evil-want-Y-yank-to-eol t)

;; Prevents esc-key from translating to meta-key in terminal mode.
(setq evil-esc-delay 0)

(defun abn-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun abn-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(evil-define-motion abn-evil-next-visual-line-5 (count)
  "Move the cursor 5 lines up."
  :type line
  (let (line-move-visual)
    (evil-next-visual-line (* 5 (or count 1)))))

(evil-define-motion abn-evil-previous-visual-line-5 (count)
  "Move the cursor 5 lines up."
  :type line
  (let (line-move-visual)
    (evil-previous-visual-line (* 5 (or count 1)))))

;; Set more useful movement commands.
(general-define-key
 :states '(normal visual motion)
 "J" 'abn-evil-next-visual-line-5
 "K" 'abn-evil-previous-visual-line-5
 "gj" 'evil-join
 "L" 'evil-end-of-line
 "C-j" 'scroll-up-command
 "C-k" 'scroll-down-command)

(general-define-key
 :states '(visual)
 ">" 'abn-shift-right-visual
 "<" 'abn-shift-left-visual)

;; Makes movement keys work on visual lines instead of actual lines.  This
;; imitates Emacs behavior rather than Vim behavior.
(general-define-key
 :states '(normal visual motion)
 (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
 (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line
 (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
 (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; http://emacs.stackexchange.com/questions/14940
(fset 'evil-visual-update-x-selection 'ignore)

;; Major modes that should default to an insert state.
(add-to-list 'evil-insert-state-modes 'git-commit-mode)

;; Magit from avsej
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard
  "L" 'magit-log-popup)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard
  "l" 'magit-log-popup
  "h" 'magit-diff-toggle-refine-hunk)

;; It's better that the default value is too small than too big.
(setq-default evil-shift-width 2)

(use-package org
  :ensure nil ;; We only want to add config options, not require org.
  :defer t
  :config
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line
    "L" 'org-end-of-line
    "t" 'org-todo
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft
    ">" 'org-metaright
    ))

(provide 'abn-evil)
