(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(setq-default evil-escape-key-sequence "jk")

;; Use a decent font.
(defun fontify-frame (frame)
  "Use appropriate font and size on FRAME."
  (interactive)
  (let ((font-family (if (member "Consolas for Powerline" (font-family-list))
                         "Consolas for Powerline"
                       (face-attribute 'default :family))))
    (when window-system
      (if (> (display-pixel-width) 2000)
          (set-frame-parameter frame 'font (format "%s 13" font-family))
        (set-frame-parameter frame 'font (format "%s 13" font-family))))))

;; Fontify current frame
(fontify-frame nil)

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

  (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
  ;; We need to add text before we can edit it.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  (unless window-system
    ;; C-i is the same as tab in the terminal
    (setq evil-want-C-i-jump nil)
    ;; I'm not sure why the above variable isn't respected. I think it's evil's
    ;; fault. I didn't see any key rebinding in spacemacs.
    (define-key evil-motion-state-map "\C-i" nil))))

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

(setq tramp-default-method "ssh")

(auto-fill-mode 1)

(with-eval-after-load 'evil-escape
  (setq evil-escape-unordered-key-sequence t))

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



