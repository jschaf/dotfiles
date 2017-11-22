;;; abn-module-tty.el --- Config for tty

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

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

(use-package abn-funcs-tty
  :ensure nil ; local package
  )

;; The inactive background is indistinguishable from the normal
;; background.
(set-face-background 'mode-line "brightmagenta")
(set-face-background 'mode-line-inactive "black")

(provide 'abn-module-tty)
;;; abn-module-tty.el ends here
