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
  :init
  :config
  (set-face-attribute
   'markdown-code-face nil
   :inherit nil
   :background nil))

(provide 'abn-module-markdown)
;;; abn-module-markdown.el ends here
