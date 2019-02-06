;;; abn-core-ui.el --- UI tweaks

;;; Commentary:
;;

;;; Code:

(defvar abn-font (pcase system-type
                   ('darwin "Consolas 13")
                   (_ "Consolas 9"))
  "The default font size to use for everything.")

(when (member "Consolas" (font-family-list))
  (set-frame-font abn-font 'keep-size)
  (add-to-list 'default-frame-alist (cons 'font abn-font)))

;; Start a clean slate.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Sets a more useful frame title, showing either a file or a buffer
;; name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

(use-package spacemacs-theme
  :defer nil ; load immediately
  :demand
  :config
  (setq-default spacemacs-theme-comment-bg nil)
  (load-theme 'spacemacs-dark 'no-confirm))

;; Control over modes displayed in the modeline.
(use-package diminish
  :defer nil ; load immediately
  :demand)

(provide 'abn-core-ui)
;;; abn-core-ui.el ends here
