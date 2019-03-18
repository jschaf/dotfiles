;;; abn-core-mac-os.el --- customization for macOS


;;; Commentary:
;;

;;; Code:

;; Suitable defaults for the Macbook pro keyboard
(setq mac-option-modifier 'control
      mac-command-modifier 'meta)

(defun abn/toggle-mac-keyboard ()
  "Switch between keyboard controls for an external keyboard and built-in keyboard"
  (interactive)
  (if (eq mac-option-modifier 'control)
      (progn
        (setq mac-option-modifier 'meta
              mac-command-modifier 'control)
        (message "Set MacOs keybindings for external keyboard."))
    (setq mac-option-modifier 'control
          mac-command-modifier 'meta)
    (message "Set MacOs keybindings for built-in keyboard.")))

(use-package browse-url
  :defer t
  :ensure nil ; built-in package
  :config
  (setq browse-url-generic-program "open")
  (setq browse-url-browser-function 'browse-url-default-macosx-browser))


(provide 'abn-core-mac-os)
;;; abn-core-mac-os.el ends here
