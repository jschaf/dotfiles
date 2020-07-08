;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Joe Schafer"
      user-mail-address "joe@jschaf.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Keybindings
(map!
 (:after counsel
  (:leader
   :desc "Execute extended command" :n "SPC" #'counsel-M-x
   )))

(defun abn/back-to-indentation-or-beginning ()
  "Move point to first non-whitespace char or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(after! evil
  (evil-define-motion abn/evil-next-visual-line-5 (count)
    "Move the cursor 5 lines up."
    :type line
    (let (line-move-visual)
      (evil-next-visual-line (* 5 (or count 1)))))

  (evil-define-motion abn/evil-previous-visual-line-5 (count)
    "Move the cursor 5 lines up."
    :type line
    (let (line-move-visual)
      (evil-previous-visual-line (* 5 (or count 1)))))

  ;; Sets more useful movement commands.
  (evil-define-key '(normal motion visual) 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "H") #'abn/back-to-indentation-or-beginning
    (kbd "J") 'abn/evil-next-visual-line-5
    (kbd "K") 'abn/evil-previous-visual-line-5
    (kbd "gj") 'evil-join
    (kbd "gh") 'evil-window-top
    (kbd "gl") 'evil-window-bottom
    (kbd "L") 'evil-end-of-line
    (kbd "C-j") 'scroll-up-command
    (kbd "C-k") 'scroll-down-command))

(after! evil-escape
  (setq evil-escape-delay 0.35))
