;;; abn-module-ivy.el --- Ivy setup

;;; Commentary:
;;

;;; Code:

(require 'use-package)

(use-package abn-funcs-ivy
  :ensure nil ; local package
  :general
  (abn/define-leader-keys
   ;; registers
   "re" 'abn/ivy-evil-registers))

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
   "fr" 'counsel-recentf

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
   "ji" 'counsel-imenu

   ;; insert
   "iu" 'counsel-unicode-char

   ;; search
   "/"  'counsel-rg
   "sp"  'counsel-rg)
  :config
  (progn
    ;; Remaps built-in commands that have a counsel replacement.
    (counsel-mode 1)))

(use-package ivy
  :diminish ivy-mode
  :general
  (abn/define-leader-keys
   "a'" 'spacemacs/ivy-available-repls
   "bb" 'ivy-switch-buffer
   "rl" 'ivy-resume)
  (:keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-h" (kbd "DEL")
   "C-S-h" help-map
   "C-l" 'ivy-alt-done
   "<escape>" 'minibuffer-keyboard-quit)

  :init
  (setq ivy-height 15))

(use-package counsel-projectile
  :general
  (abn/define-leader-keys
   "p SPC" 'counsel-projectile
   "pb" 'counsel-projectile-switch-to-buffer
   "pd" 'counsel-projectile-find-dir
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile-find-file
   "pr" 'projectile-recentf)
  :init
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'counsel-projectile-find-file)))

;; counsel-M-x will use smex if available.
(use-package smex
  :defer t
  :init
  (setq smex-save-file (concat abn-cache-dir "/smex-items")))

(use-package swiper
  :ensure
  :general
  (;; Current global keymap.
   "\C-s" 'swiper

   :keymaps 'abn-leader-map
   "ss" 'swiper
   "sS" 'spacemacs/swiper-region-or-symbol
   "sb" 'swiper-all
   "sB" 'spacemacs/swiper-all-region-or-symbol))

(provide 'abn-module-ivy)
;;; abn-module-ivy.el ends here
