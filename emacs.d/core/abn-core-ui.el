;;; abn-core-ui.el --- UI tweaks

;;; Commentary:
;;

;;; Code:

(pcase system-type
  ('darwin (set-frame-font "Consolas 13" 'keep-size))
  (_ (set-frame-font "Consolas 17" 'keep-size)))

;; In a tty the tool-bar-mode does not properly auto-load, and is
;; already disabled anyway.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Silence ad-handle-definition about advised functions getting redefined.
(setq ad-redefinition-action 'accept)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Disables the startup screen.
(setq inhibit-startup-screen t)

;; Enables nice scrolling.
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Sets a more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(load-theme 'spacemacs-dark 'no-confirm)

(provide 'abn-core-ui)
;;; abn-core-ui.el ends here
