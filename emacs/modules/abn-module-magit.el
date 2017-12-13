;;; abn-module-magit.el --- Config for magit

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-magit
  :ensure nil ; local package
  )

(use-package magit
  :defer t
  :config
  (define-key magit-mode-map (kbd "SPC") abn-leader-map)
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'abn-module-magit)
;;; abn-module-magit.el ends here
