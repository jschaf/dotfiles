;;; abn-module-editing.el --- Setup editing

;;; Commentary:
;;

;;; Code:

;; Formatting
(use-package abn-funcs-editing
  :ensure nil ; local package
  :commands
  (abn/tentatively-start-atomic-chrome-server
   abn/back-to-indentation-or-beginning
   abn/get-current-buffer-file-path
   abn/goto-middle-of-line
   abn/save-buffers-visiting-files)
  :bind
  (:map abn-leader-map
   ("b!" . abn/shell-command-on-buffer)
   ("ii" . abn/insert-single-character)
   ("ia" . abn/append-single-character)
   ("jo" . open-line)
   ("j=" . abn/indent-region-or-buffer)
   ("jS" . abn/split-and-new-line)
   ("jk" . abn/evil-goto-next-line-and-indent)
   ("tl" . toggle-truncate-lines)
   ("yf" . abn/yank-file-path)
   ("yd" . abn/yank-directory-path)
   ("ym" . abn/yank-last-message))
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "gm"  #'abn/goto-middle-of-line
      "H"  #'abn/back-to-indentation-or-beginning)))

(use-package atomic-chrome
  :defer 2
  :ensure t
  :config
  ;; Start immediately
  (abn/tentatively-start-atomic-chrome-server)
  (run-with-idle-timer
   10 ; seconds
   'repeat ; Repeat after emacs is idle for 10 seconds.
   #'abn/tentatively-start-atomic-chrome-server))

(use-package conf-mode
  :defer t
  :ensure nil ; built-in package
  :config
  (progn
    ;; Interferes with the agenda.
    (define-key conf-mode-map (kbd "C-c C-a") nil)))

;; File input and output commands for Emacs.
(use-package files
  :demand
  :ensure nil ; built-in package
  :config
  (add-hook 'focus-out-hook 'abn/save-buffers-visiting-files))

(use-package flycheck
  :defer t)

;; Inserts dummy text.
(use-package lorem-ipsum
  :defer t
  :bind
  (:map abn-leader-map
   ("ill" . lorem-ipsum-insert-list)
   ("ilp" . lorem-ipsum-insert-paragraphs)
   ("ils" . lorem-ipsum-insert-sentences))
  :init
  (abn-declare-prefix "il" "lorem ipsum"))

;; Moves the current line or region up or down.
(use-package move-text
  :defer t
  :bind
  (:map evil-normal-state-map
   ("[ e" . move-text-up)
   ("] e" . move-text-down)))

(use-package simple
  :defer 1
  :ensure nil ; built-in package
  :diminish visual-line-mode
  :config
  ;; Add period to chars that start auto-fill.
  (aset auto-fill-chars ?. t))

;; Unfills paragraphs.
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

(use-package uniquify
  :defer 2
  :ensure nil ; built-in package
  :config
  ;; When having windows with repeated filenames, uniquify them by
  ;; folder rather than the default suffix <2>, <3>, ..., <n>.
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
	;; Ignore special buffers.
	uniquify-ignore-buffers-re "^\\*"))

(use-package wgrep
  :ensure t
  :defer t)

(use-package whitespace
  :defer 1
  :ensure nil ; built-in package
  :diminish whitespace-mode)

;; Unobtrusively trim extra white-space *ONLY* in edited lines.
(use-package ws-butler
  :defer 2
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(provide 'abn-module-editing)
;;; abn-module-editing.el ends here
