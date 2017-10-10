;;; abn-module-mode-line.el --- Config for mode-line

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-mode-line
  :ensure nil ; local package
  )

;; Disable dashes on terminals at the end of the mode line.
(setq mode-line-end-spaces
      '(:eval (unless (display-graphic-p) " ")))

;; Defaults to a dash on non-graphical display.
(setq mode-line-front-space " ")

(progn
  (setq-default
   mode-line-format
   '(
     "%e" ; Error message about full memory.
     mode-line-front-space
     ;; Disable mule-info.  We use UTF-8 everywhere. It looks like UU-:
     ;; mode-line-mule-info
     mode-line-client
     mode-line-modified
     "  "
     ;; We rarely care about the auto-compile state.
     ;; mode-line-auto-compile
     ;; mode-line-remote
     ;; Don't care about which frame we're on.
     ;; mode-line-frame-identification

     mode-line-buffer-identification
     "  "
     mode-line-position
     evil-mode-line-tag
     (vc-mode vc-mode)
     "  "
     mode-line-modes

     ;; Defaults to which-func-mode and global-mode-string.
     mode-line-misc-info
     mode-line-end-spaces))
  (force-mode-line-update))

(provide 'abn-module-mode-line)
;;; abn-module-mode-line.el ends here
