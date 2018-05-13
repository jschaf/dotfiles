;;; abn-module-tty.el --- Config for tty

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-tty
  :ensure nil ; local package
  :commands
  (abn-tty/yank
   abn-tty/buffer-substring-terminal-filter))

;; Change the cursor display in a terminal emacs.
(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :demand
  :init
  (setq evil-visual-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-emacs-state-cursor 'hbar)
  :config
  (evil-terminal-cursor-changer-activate)

  (setq-default filter-buffer-substring-function
                ;; #'buffer-substring--filter
                #'abn-tty/buffer-substring-terminal-filter
                ))

;; Commands to send text to the host terminal's clipboard.
(use-package osc52
  :ensure nil ; local package
  :commands
  (osc52-select-text))

;; The inactive background is indistinguishable from the normal
;; background.
(set-face-background 'mode-line "#303030")
(set-face-background 'mode-line-inactive "#1c1c1c")

(provide 'abn-module-tty)
;;; abn-module-tty.el ends here
