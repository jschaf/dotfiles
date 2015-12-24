(add-to-list 'dotspacemacs-configuration-layer-path
             "~/.dotfiles/spacemacs-layers/")

;; Mute def-advice warnings, see
;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

(defun dotspacemacs/init ()
  (setq-default evil-escape-key-sequence "jk"
                dotspacemacs-editing-style 'hybrid
                dotspacemacs-leader-key "SPC"
                dotspacemacs-emacs-leader-key "M-m"
                dotspacemacs-major-mode-leader-key ","
                dotspacemacs-command-key ":"))

(defun dotspacemacs/layers ()
  "Layers to use for Spacemacs."
  (setq-default dotspacemacs-configuration-layers
                '((auto-completion
                   :variables
                   auto-completion-return-key-behavior 'complete
                   auto-completion-tab-key-behavior 'cycle
                   auto-completion-enable-help-tooltip t
                   auto-completion-enable-snippets-in-popup t
                   auto-completion-enable-sort-by-usage t)
                  dash
                  emacs-lisp
                  ess
                  (git :variables
                       git-magit-status-fullscreen t
                       git-enable-github-support t)
                  joe
                  markdown
                  mu4e
                  org
                  rust
                  python
                  restclient
                  spell-checking
                  syntax-checking
                  typescript
                  version-control)))

(setq-default dotspacemacs-default-font '("Consolas"
                                          :size 13
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.55))
(defun dotspacemacs/user-config ()
  "Personal config for Spacemacs."
  (setq select-enable-clipboard t)

  (setq org-src-fontify-natively t)
  (spacemacs/toggle-auto-fill-mode-on)

  (setq-default abbrev-file-name "~/.dotfiles/abbrev_defs")

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
 '(abbrev-file-name "/home/joe/.emacs.d/.cache/abbrev_defs")
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(magit-diff-refine-hunk (quote all))
 '(org-drill-learn-fraction 0.3)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-drill)))
 '(package-selected-packages
   (quote
    (orgit diminish openwith jinja2-mode hydra hl-todo toc-org org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets gnuplot flycheck-rust flycheck-pos-tip flycheck persistent-scratch git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter pyvenv pytest pyenv-mode pip-requirements hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode restclient ess-smart-equals ess-R-object-popup ess-R-data-view ess company-statistics key-chord helm-bibtex ebib rust-mode json-mode help-fns+ bind-map htmlize auto-complete highlight-parentheses gh-md ac-ispell ws-butler persp-mode lorem-ipsum evil-magit evil-indent-plus ace-jump-helm-line smeargle helm-core restart-emacs helm-flx auto-compile beacon zeal-at-point package-build helm-company evil-mc json-reformat tss spacemacs-theme racer pcre2el macrostep helm-dash git-timemachine auto-yasnippet company magit which-key quelpa spaceline esup company-racer deferred mmm-mode markdown-toc markdown-mode diff-hl window-numbering volatile-highlights vi-tilde-fringe smooth-scrolling rfringe rainbow-delimiters powerline popup paradox page-break-lines neotree multi-term move-text monokai-theme linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-anything highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flyspell helm-descbinds helm-c-yasnippet helm-ag guide-key-tip google-translate golden-ratio fringe-helper flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-terminal-cursor-changer evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav buffer-move base16-theme auto-highlight-symbol auto-dictionary aggressive-indent adaptive-wrap ace-window ace-link ace-jump-mode avy names anzu iedit smartparens highlight flx pos-tip guide-key s popwin yasnippet projectile helm async parent-mode spinner pkg-info epl evil-leader evil use-package bind-key dash)))
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((reftex-default-bibliography . hammelburg\.bib)
     (reftex-default-bibliography . somalia\.bib)
     (reftex-cite-format
      (13 . "[@%l]"))
     (zotero-collection .
                        #("1" 0 1
                          (name "Somalia")))
     (reftex-default-bibliography "somalia.bib")
     (my:use-jinja-for-html-p . t))))
 '(spacemacs-theme-comment-bg nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
