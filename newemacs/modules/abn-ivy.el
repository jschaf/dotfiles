;;; abn-ivy.el --- Ivy setup

;;; Commentary:
;;

;;; Code:

(require 'use-package)

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-height 15)

  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
  (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<escape>")
    'minibuffer-keyboard-quit))


(use-package counsel
  :diminish counsel-mode
  :bind
  (;; Current global keymap
   ("M-x" . counsel-M-x)

   :map abn-leader-map
   ("SPC" . counsel-M-x)

   ;; files
   ("ff" . counsel-find-file)
   ("fL" . counsel-locate)

   ;; help
   ("?"  . counsel-descbinds)
   ("hdf". counsel-describe-function)
   ("hdm". spacemacs/describe-mode)
   ("hdv". counsel-describe-variable)
   ("hR" . spacemacs/counsel-search-docs)

   ;; register/ring
   ("ry" . counsel-yank-pop)

   ;; jumping
   ("sj" . counsel-imenu)

   ;; insert
   ("iu" . counsel-unicode-char)

   ;; search
   ("/"  . counsel-rg)
   ("sp"  . counsel-rg))
  :config
  (progn
    ;; Remaps built-in commands that have a counsel replacement.
    (counsel-mode 1)))

(use-package swiper
  :ensure
  :bind
  (;; Current global keymap.
   ("\C-s" . swiper)

   :map abn-leader-map
   ("ss" . swiper)
   ("sS" . spacemacs/swiper-region-or-symbol)
   ("sb" . swiper-all)
   ("sB" . spacemacs/swiper-all-region-or-symbol))
  )

(provide 'abn-ivy)
;;; abn-ivy.el ends here
