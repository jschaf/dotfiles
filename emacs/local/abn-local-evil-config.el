;;; abn-local-evil-config.el --- Personal config for evil


;;; Commentary:
;; 

;;; Code:
(require 'evil)

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
(general-define-key
 :states '(normal visual motion)
  "gj" 'evil-join
  "gh" 'evil-window-top
  "gl" 'evil-window-bottom
  "L" 'evil-end-of-line
  "C-j" 'scroll-up-command
  "C-k" 'scroll-down-command)

;; Makes movement keys work on visual lines instead of actual lines.
;; This imitates Emacs behavior rather than Vim behavior.
(general-define-key
 :states '(normal visual motion)
  (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line
  (kbd "<remap> <evil-next-line>") 'evil-next-visual-line
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(provide 'abn-local-evil-config)
;;; abn-local-evil-config.el ends here
