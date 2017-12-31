;;; abn-module-dired.el --- Config for dired

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-dired
  :ensure nil ; local package
  :defer t)

(use-package dired
  :ensure nil ; built-in package
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map "gr" 'revert-buffer))
  :config
  (define-key dired-mode-map (kbd "SPC") abn-leader-map))


(use-package dired-x
  :ensure nil ; built-in package
  :bind
  (:map abn-leader-map
   ("fj" . dired-jump)
   ("jd" . dired-jump)
   ("jD" . dired-jump-other-window)))

(use-package dired
  :ensure nil ; built-in package
  :bind
  (:map abn-leader-map
   ("ad" . dired)))

(provide 'abn-module-dired)
;;; abn-module-dired.el ends here
