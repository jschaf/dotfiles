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
   abn/goto-middle-of-line)
  :bind
  (:map abn-leader-map
   ("b!" . abn/shell-command-on-buffer)
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
    (evil-define-key 'motion
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
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'motion
      "[ e" 'move-text-up
      "] e" 'move-text-down)))

(use-package simple
  :defer 1
  :ensure nil ; built-in package
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
  :defer t)

(use-package whitespace
  :defer 1
  :ensure nil ; built-in package
  :diminish whitespace-mode
  :config
  (setq-default whitespace-line-column 80)
  (setq-default whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook #'whitespace-mode))

(provide 'abn-module-editing)
;;; abn-module-editing.el ends here
