;;; abn-module-markdown.el --- Config for markdown

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-markdown
  :ensure nil ; local package
  )

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (set-face-attribute 'markdown-code-face
                      nil ; all frames
                      :inherit nil
                      :background nil)
  (setq markdown-fontify-code-blocks-natively t))

(provide 'abn-module-markdown)
;;; abn-module-markdown.el ends here
