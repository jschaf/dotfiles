(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(setq-default evil-escape-key-sequence "jk")

(use-package evil
  :init
  (progn
  ;; Make movement keys work on visual lines instead of acutal lines.
  ;; This imitates Emacs behavior rather than Vim behavior.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")
    'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>")
    'evil-previous-visual-line)
))


(use-package typescript
  :init
  (progn
    (add-to-list 'compilation-error-regexp-alist 'typescript)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(typescript "^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
                              1 2 3 nil 1))


    (add-to-list 'compilation-error-regexp-alist 'typescript-lint)
    ;; ornament/static/js/main.ts[176, 34]: expected parameter: 'error' to have a typedef
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(typescript-lint "^\\(.+?\\)\\[\\([[:digit:]]+\\), \\([[:digit:]]+\\)\\]: \\(.*\\)$"
                                   1 2 3 nil 1))))
