;;; abn-core-mac-os.el --- customization for macOS


;;; Commentary:
;;

;;; Code:

(setq mac-option-modifier 'control
      mac-command-modifier 'meta)

(use-package browse-url
  :defer t
  :ensure nil ; built-in package
  :config
  (setq browse-url-generic-program "open")
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))


(provide 'abn-core-mac-os)
;;; abn-core-mac-os.el ends here
