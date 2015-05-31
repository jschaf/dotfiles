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
  (evil-set-initial-state 'dired-mode 'emacs)

  ;; We need to add text before we can edit it.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)
))


;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)


(setq tramp-default-method "ssh")

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; .dir-local.el tweaks
;;
;; See http://stackoverflow.com/questions/5147060/ for explanation of
;; why we're creating a new hook.  tl;dr: wierd interaction between
;; `set-auto-mode' and `hack-local-variables'
(add-hook 'hack-local-variables-hook 'my:run-local-vars-mode-hook)
(defun my:run-local-vars-mode-hook ()
  "Hook for all major-modes after processing local variables.
Creates a hook for all major modes.
e.g. `python-mode-local-vars-hook',
`emacs-lisp-mode-local-vars-hook'"
  (run-hooks (intern (format "%s-local-vars-hook" (symbol-name major-mode)))))

(defvar my:use-jinja-for-html-p nil
  "Use `jinja2-mode' if non-nil, otherwise `html-mode'.
Primarily for use in .dir-locals.el")

(defun my:maybe-choose-jinja2-mode ()
  (when my:use-jinja-for-html-p
    (jinja2-mode)))

(add-hook 'html-mode-local-vars-hook 'my:maybe-choose-jinja2-mode)

(use-package typescript
  :init
  (progn
    (with-eval-after-load 'compile
      (add-to-list 'compilation-error-regexp-alist 'typescript)
      (add-to-list 'compilation-error-regexp-alist-alist 
                   '(typescript "^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
                                1 2 3 nil 1))


      (add-to-list 'compilation-error-regexp-alist 'typescript-lint)
      ;; ornament/static/js/main.ts[176, 34]: expected parameter: 'error' to have a typedef
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(typescript-lint "^\\(.+?\\)\\[\\([[:digit:]]+\\), \\([[:digit:]]+\\)\\]: \\(.*\\)$"
                                     1 2 3 nil 1)))))
