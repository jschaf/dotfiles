;;; abn-module-avy.el --- Avy module


;;; Commentary:
;;

;;; Code:

(use-package avy
  :commands (abn/avy-open-url abn/avy-goto-url avy-pop-mark)
  :general
  (:keymaps 'abn-leader-map
   "jb" 'avy-pop-mark
   "jj" 'evil-avy-goto-char
   "jJ" 'evil-avy-goto-char-2
   "jl" 'evil-avy-goto-line
   "ju" 'abn/avy-goto-url
   "jw" 'evil-avy-goto-word-or-subword-1
   "xo" 'abn/avy-open-url)
  :init
  (setq avy-all-windows 'all-frames)
  (setq avy-background t)

  :config
  (progn
    (defun abn/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy--generic-jump "https?://" nil 'pre))

    (defun abn/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
	(abn/avy-goto-url)
	(browse-url-at-point)))))

(provide 'abn-module-avy)
;;; abn-module-avy.el ends here
