(defun my:evil-keybindings ()
  (interactive)
  (define-key evil-normal-state-map "\M-k" 'spacemacs/evil-smart-doc-lookup)
  (define-key evil-normal-state-map "K" 'my:evil-previous-visual-line-5)
  (cl-loop for (key . func) in
           `(("J" . my:evil-next-visual-line-5)
             ("K" . my:evil-previous-visual-line-5)
             ("gj" . evil-join)
             ("H" . my:back-to-indentation-or-beginning)
             ("L" . evil-end-of-line)
             ("\C-j" . scroll-up-command)
             ("\C-k" . scroll-down-command))
           do
           (define-key evil-normal-state-map key func)
           (define-key evil-visual-state-map key func)
           (define-key evil-motion-state-map key func)))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)
