;;; abn-module-markup.el --- Config for markup languages

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package abn-funcs-markup
  :ensure nil ; local package
  )

(use-package htmlize
  :defer t)

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  ;; Avoid ~1.7 second delay by using `char-displayable-p'.  See
  ;; https://github.com/jrblevin/markdown-mode/issues/264
  (setq markdown-url-compose-char ?∞)
  (setq markdown-blockquote-display-char "▌")
  (setq markdown-hr-display-char ?─)
  (setq markdown-definition-display-char ?⁘)

  :config
  (set-face-attribute 'markdown-code-face
                      nil ; all frames
                      :inherit nil
                      :background nil)
  (setq markdown-fontify-code-blocks-natively t))

(provide 'abn-module-markup)
;;; abn-module-markdown.el ends here
