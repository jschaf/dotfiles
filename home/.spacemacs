(add-to-list 'dotspacemacs-configuration-layer-path
             "~/.dotfiles/emacs-layers/")

(defun dotspacemacs/init ()
  (setq-default evil-escape-key-sequence "jk")
  (setq-default dotspacemacs-configuration-layers
                '(auto-completion
                  emacs-lisp
                  (git :variables
                       git-magit-status-fullscreen t
                       git-enable-github-support t
                       git-gutter-use-fringe t)
                  joe
                  python
                  typescript))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(package-selected-packages
   (quote
    (zenburn-theme window-numbering volatile-highlights vi-tilde-fringe smooth-scrolling rfringe rainbow-delimiters powerline popup paradox page-break-lines neotree multi-term move-text monokai-theme linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-anything highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flyspell helm-descbinds helm-c-yasnippet helm-ag guide-key-tip google-translate golden-ratio fringe-helper flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav buffer-move base16-theme auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link ace-jump-mode avy names anzu iedit smartparens highlight flx pos-tip guide-key s popwin yasnippet projectile helm async parent-mode spinner pkg-info epl evil-leader evil use-package bind-key dash)))
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values (quote ((my:use-jinja-for-html-p . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


