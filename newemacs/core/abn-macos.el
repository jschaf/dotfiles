;;; abn-macos.el --- customization for macOS

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'control
        mac-command-modifier 'meta))
