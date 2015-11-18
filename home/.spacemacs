(add-to-list 'dotspacemacs-configuration-layer-path
             "~/.dotfiles/spacemacs-layers/")

;; Mute def-advice warnings, see
;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

(defun dotspacemacs/init ()
  (setq-default evil-escape-key-sequence "jk"
                dotspacemacs-themes '(solarized-dark)
                dotspacemacs-editing-style 'hybrid
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "M-m"
                dotspacemacs-major-mode-leader-key ","
                dotspacemacs-command-key ":"))

(defun dotspacemacs/layers ()
  (setq-default dotspacemacs-configuration-layers
                '(auto-completion
                  dash
                  emacs-lisp
                  (git :variables
                       git-magit-status-fullscreen t
                       git-enable-github-support t)
                  joe
                  markdown
                  rust
                  spell-checking
                  typescript
                  version-control)))

(setq-default dotspacemacs-default-font '("Consolas"
                                          :size 13
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.55))
(defun dotspacemacs/user-config ()
  (setq select-enable-clipboard t
        x-select-enable-primary t)

  (spacemacs/toggle-auto-fill-mode-on)

  ;; Copy/Paste in the terminal is huge pain. See
  ;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
  (unless window-system
    (require 'mouse)
    (global-set-key [mouse-4] (lambda ()
                                (interactive)
                                (scroll-down 1)))
    (global-set-key [mouse-5] (lambda ()
                                (interactive)
                                (scroll-up 1)))
    (xterm-mouse-mode)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)

    (defun xsel-cut-function (text &optional push)
      ;; Insert text to temp-buffer, and "send" content to xsel stdin
      (with-temp-buffer
        (insert text)
        ;; I prefer using the "clipboard" selection (the one the
        ;; typically is used by c-c/c-v) before the primary selection
        ;; (that uses mouse-select/middle-button-click)
        (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    ;; Call back for when user pastes
    (defun xsel-paste-function()
      ;; Find out what is current selection by xsel. If it is different
      ;; from the top of the kill-ring (car kill-ring), then return
      ;; it. Else, nil is returned, so whatever is in the top of the
      ;; kill-ring will be used.
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)

    (setq dotspacemacs-mode-line-unicode-symbols nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(package-selected-packages
   (quote
    (htmlize auto-complete highlight-parentheses gh-md ac-ispell ws-butler persp-mode lorem-ipsum evil-magit evil-indent-plus ace-jump-helm-line smeargle helm-core restart-emacs helm-flx auto-compile beacon zeal-at-point package-build helm-company evil-mc json-reformat tss spacemacs-theme racer pcre2el macrostep helm-dash git-timemachine auto-yasnippet company magit which-key quelpa spaceline esup company-racer deferred mmm-mode markdown-toc markdown-mode diff-hl window-numbering volatile-highlights vi-tilde-fringe smooth-scrolling rfringe rainbow-delimiters powerline popup paradox page-break-lines neotree multi-term move-text monokai-theme linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-anything highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flyspell helm-descbinds helm-c-yasnippet helm-ag guide-key-tip google-translate golden-ratio fringe-helper flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav buffer-move base16-theme auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link ace-jump-mode avy names anzu iedit smartparens highlight flx pos-tip guide-key s popwin yasnippet projectile helm async parent-mode spinner pkg-info epl evil-leader evil use-package bind-key dash)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values (quote ((my:use-jinja-for-html-p . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
