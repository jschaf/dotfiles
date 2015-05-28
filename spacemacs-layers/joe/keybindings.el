(cl-loop for (key . func) in
         `(("J" . my:evil-next-visual-line-5)
           ("K" . my:evil-previous-visual-line-5)
           ("\M-j" . my:evil-next-visual-line-3)
           ("\M-k" . my:evil-previous-visual-line-3)
           ("gj" . evil-join)
           ("H" . my:back-to-indentation-or-beginning)
           ("L" . evil-end-of-line)
           ("\C-j" . scroll-up-command)
           ("\C-k" . scroll-down-command))
         do
         (define-key evil-normal-state-map key func)
         (define-key evil-visual-state-map key func)
         (define-key evil-motion-state-map key func))
