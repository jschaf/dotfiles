;;; abn-module-ivy.el --- Ivy setup

;;; Commentary:
;;

;;; Code:

(require 'use-package)

(use-package ivy
  :diminish ivy-mode
  :general
  (:keymaps 'ivy-minibuffer-map
            "C-j" 'ivy-next-line
            "C-k" 'ivy-previous-line
            "C-h" (kbd "DEL")
            "C-S-h" help-map
            "C-l" 'ivy-alt-done
            "<escape>" 'minibuffer-keyboard-quit)

  :config
  (setq ivy-height 15))


(use-package counsel
  :diminish counsel-mode
  :general
  (;; Current global keymap
   "M-x" 'counsel-M-x

   :keymaps 'abn-leader-map
   "SPC" 'counsel-M-x

   ;; files
   "ff" 'counsel-find-file
   "fL" 'counsel-locate

   ;; help
   "?"  'counsel-descbinds
   "hdf" 'counsel-describe-function
   "hdm" 'spacemacs/describe-mode
   "hdv" 'counsel-describe-variable
   "hR" 'spacemacs/counsel-search-docs

   ;; register/ring
   "ry" 'counsel-yank-pop

   ;; jumping
   "sj" 'counsel-imenu

   ;; insert
   "iu" 'counsel-unicode-char

   ;; search
   "/"  'counsel-rg
   "sp"  'counsel-rg)
  :config
  (progn
    ;; Remaps built-in commands that have a counsel replacement.
    (counsel-mode 1)))

(use-package swiper
  :ensure
  :general
  (;; Current global keymap.
   "\C-s" 'swiper

   :keymaps 'abn-leader-map
   "ss" 'swiper
   "sS" 'spacemacs/swiper-region-or-symbol
   "sb" 'swiper-all
   "sB" 'spacemacs/swiper-all-region-or-symbol)
  )

(provide 'abn-module-ivy)
;;; abn-module-ivy.el ends here
