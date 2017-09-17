;;; abn-evil.el --- Basic evil-mode configuration.

;;; Code:

;;; goto-chg lets you use the g-; and g-, to go to recent changes
;;; evil-visualstar enables searching visual selection with *
;;; evil-numbers enables vim style numeric incrementing and decrementing
(abn-require-packages
 '(evil
   evil-escape
   goto-chg
   evil-surround
   evil-visualstar
   evil-numbers))

(message "here ")
(require 'cl)
(require 'evil-visualstar)

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

;; evil-want-Y-yank-to-eol must be set via customize to have an effect
(customize-set-variable 'evil-want-Y-yank-to-eol t)

;; prevent esc-key from translating to meta-key in terminal mode
(setq evil-esc-delay 0)

(evil-mode 1)
(global-evil-surround-mode 1)

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

(general-define-key
 :states '(normal visual motion)
 "J" 'abn-evil-next-visual-line-5
 "K" 'abn-evil-previous-visual-line-5
 "gj" 'evil-join
 "L" 'evil-end-of-line
 "\C-j" 'scroll-up-command
 "\C-k" 'scroll-down-command)

;; Makes movement keys work on visual lines instead of actual lines.  This
;; imitates Emacs behavior rather than Vim behavior.
(general-define-key
 :states '(normal)
 (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
 (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line
 (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
 (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; http://emacs.stackexchange.com/questions/14940
(fset 'evil-visual-update-x-selection 'ignore)

;; Major modes that should default to an insert state.
(add-to-list 'evil-insert-state-modes 'git-commit-mode)

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t))

(define-key evil-visual-state-map (kbd ">") 'abn-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'abn-shift-left-visual)

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

(defun abn-evil-key-bindings-for-org ()
  (message "Defining evil key bindings for org")
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    ))
(abn-evil-key-bindings-for-org)
(provide 'abn-evil)
