;;; config.el --- Configuration for Joe's private layer.


;;; Commentary:
;;

;;; Code:

(when (eq system-type 'darwin)
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)

  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)

  (defvar my:mac-modifier-state 'mac
    "Toggle between BUILT-IN and USB.")

  (defun my:toggle-mac-modifiers ()
    (interactive)
    (if (eq my:mac-modifier-state 'usb)
        (progn
          (setq my:mac-modifier-state 'built-in
                mac-option-modifier 'control
                mac-command-modifier 'meta)
          (message "Mac modifier keys set for Mac keyboard."))

      (setq my:mac-modifier-state 'usb
            mac-option-modifier 'meta
            mac-command-modifier 'super)
      (message "Mac modifier keys set for USB keyboard.")))

  (setq my:mac-modifier-state 'built-in
        mac-option-modifier 'control
        mac-command-modifier 'meta)
  (message "Mac modifier keys set for Mac keyboard."))

(defun my:back-to-indentation-or-beginning ()
  "Move point to first non-whitespace char or `beginning-of-line'."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun unfill-paragraph ()
  "Replace newline characters with a single space."
  (interactive)
  (let ((fill-column 90002000))
    (if mark-active
        (fill-region (region-beginning) (region-end))
      (fill-paragraph))))

(defun my:new-blah-buffer ()
  "Open up a guaranteed new blah (scratch) buffer."
  (interactive)
  (switch-to-buffer (cl-loop for num from 0
                             for name = (format "blah-%03i" num)
                             while (get-buffer name)
                             finally return name)))

(defun my:switch-to-blah-buffer ()
  "Switch to a blah buffer, or create a new one."
  (interactive)
  (cl-loop for buffer in (buffer-list)
           if (string-match "blah-.+" (buffer-name buffer))
           return (switch-to-buffer buffer)
           finally do (my:new-blah-buffer)))

(setq-default evil-escape-key-sequence "jk")

(setq-default sentence-end-double-space t)

(defun my:nuke-all-buffers ()
  "Kill all buffers, leaving only *scratch* only."
  (interactive)
  (mapc
   (lambda (buffer)
     (kill-buffer buffer))
   (buffer-list))
  (delete-other-windows))

;; Use a decent font.
(defun fontify-frame (frame)
  "Use appropriate font and size on FRAME."
  (interactive)
  (let ((font-family (if (member "Consolas for Powerline" (font-family-list))
                         "Consolas for Powerline"
                       (face-attribute 'default :family))))
    (when window-system
      (if (> (display-pixel-width) 2000)
          (set-frame-parameter frame 'font (format "%s 13" font-family))
        (set-frame-parameter frame 'font (format "%s 13" font-family))))))

;; Fontify current frame
(fontify-frame nil)

(require 'core-keybindings)

(with-eval-after-load 'lisp-mode
  (require 'pp)
  (defun my-pp-eval-last-sexp-in-current-buffer ()
    (interactive)
    (pp-eval-last-sexp t))
  (spacemacs/set-leader-keys-for-major-mode 'lisp-interaction-mode
    "ep" 'my-pp-eval-last-sexp-in-current-buffer
    "eP" 'pp-eval-last-sexp))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

;; Follow symlinks to source controlled files without prompting.
(setq vc-follow-symlinks t)

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; .dir-local.el tweaks
;;
;; See http://stackoverflow.com/questions/5147060/ for explanation of
;; why we're creating a new hook.  tl;dr: wierd interaction between
;; `set-auto-mode' and `hack-local-variables'
(add-hook 'hack-local-variables-hook 'my:run-local-vars-mode-hook)
(defun my:run-local-vars-mode-hook ()
  "Hook for all major-modes after processing local variables.
Creates a hook for all major modes.
e.g. `python-mode-local-vars-hook',
`emacs-lisp-mode-local-vars-hook'"
  (run-hooks (intern (format "%s-local-vars-hook" (symbol-name major-mode)))))

(defvar my:use-jinja-for-html-p nil
  "Use `jinja2-mode' if non-nil, otherwise `html-mode'.
Primarily for use in .dir-locals.el")

(defun my:maybe-choose-jinja2-mode ()
  (when my:use-jinja-for-html-p
    (jinja2-mode)))

(add-hook 'html-mode-local-vars-hook 'my:maybe-choose-jinja2-mode)

;; Custom keymaps
(defvar joe-map (make-keymap))

(defun joe/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under `joe-map'.
The key for `joe-map' is `joe-leader-key'.  KEY should be a
string suitable for passing to `kbd', and it should not include
the leaders.  DEF is most likely a quoted command.  See
`define-key' for more information about the possible choices for
DEF.  This function simply uses `define-key' to add the bindings.

For convenience, BINDINGS is additional KEY DEF pairs.  For
example,

\(joe/set-leader-keys
   \"a\" 'command1
   \"c\" 'command2
   \"bb\" 'command3\)"
  (while key
    (define-key joe-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(spacemacs/set-leader-keys
  "," joe-map)

(defun my:find-file-builder (name path)
  "Create function to open PATH."
  `(defun ,name()
     (interactive)
     (find-file ,path)))

(loop for (binding name path) in
      '(("fep" my:open-joe-packages "~/.dotfiles/layers/joe/packages.el")
        ("fec" my:open-joe-config "~/.dotfiles/layers/joe/config.el")
        ("fes" my:open-biz-plan "~/org/swift-plaque-business-plan.org")
        ("gg" my:open-gtd "~/gdrive/org/gtd.org")
        ("gw" my:open-goog "~/gdrive/gorg/goog.org")
        ("gs" my:open-sandlot "~/gdrive/gorg/sandlot.org")
        ("gm" my:open-my-org "~/.dotfiles/layers/joe/local/my-org.el")
        ("feo" my:open-org-drill "~/gdrive/drill/programming.org"))
      do
      (let ((fn (my:find-file-builder name path)))
        (eval fn)
        (joe/set-leader-keys binding name)))

(defun operate-on-point-or-region (fn)
  "Get the current unspaced string at point.
Replace with the return value of the function FN"
  (let (pos1 pos2 meat excerpt)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq excerpt (buffer-substring-no-properties pos1 pos2))
    (setq meat (funcall fn excerpt))
    (delete-region pos1 pos2)
    (insert  meat)))

(defun my:replace-or-add-to-alist (alist-var elem)
  "Replace the first entry whose `car' `equal's (car ELEM) in ALIST-VAR with ELEM.
ALIST-VAR must be a symbol.  If no \(car entry\) in ALIST-VAR
equals the `car' of ELEM, then prepend ELEM to ALIST-VAR.

The return value is ELEM.

\(my:replace-or-add-to-alist 'an-alist '(\"key\" \"data\")\)"
  (let ((alist (symbol-value alist-var)))
    (if (assoc (car elem) alist)
        (setcdr (assoc (car elem) alist)
                (cdr elem))
      (set alist-var (cons elem alist)))
    elem))

(defun my:pretty-print-xml-region (begin end)
  "Pretty format XML markup in region BEGIN END."
  (interactive "r")
  (save-excursion
    ;; split <foo><bar> or </foo><bar>, but not <foo></foo>
    (goto-char begin)
    (while (search-forward-regexp ">[ \t]*<[^/]" end t)
      (backward-char 2) (insert "\n") (incf end))
    ;; split <foo/></foo> and </foo></foo>
    (goto-char begin)
    (while (search-forward-regexp "<.*?/.*?>[ \t]*<" end t)
      (backward-char) (insert "\n") (incf end))
    ;; put xml namespace decls on newline
    (goto-char begin)
    (while (search-forward-regexp "\\(<\\([a-zA-Z][-:A-Za-z0-9]*\\)\\|['\"]\\) \\(xmlns[=:]\\)" end t)
      (goto-char (match-end 0))
      (backward-char 6) (insert "\n") (incf end))
    (indent-region begin end nil)))

(with-eval-after-load 's
  (defun my:snake-case-at-point-or-region ()
    "Snake_case the current word or text selection."
    (interactive)
    (operate-on-point-or-region 's-snake-case))

  (defun my:dasherise-at-point-or-region ()
    "Dasherise-the-current CamelCase or snake_case word or text selection."
    (interactive)
    (operate-on-point-or-region 's-dashed-words))

  (defun my:upper-camelcase-at-point-or-region ()
    "UpperCamelCaseTheCurrent dashed-or-snake_case_words or any words in text selection."
    (interactive)
    (operate-on-point-or-region 's-upper-camel-case))

  (defun my:lower-camelcase-at-point-or-region ()
    "LowerCamelCaseTheCurrent dashed or snake_case word or any words in text selection."
    (interactive)
    (operate-on-point-or-region 's-lower-camel-case))

  (defun my:humanize-at-point-or-region ()
    "Humanize variable names, insert spaces instead of - or _ or un-CamelCase humps to spaced words."
    (interactive)
    (operate-on-point-or-region 's-capitalized-words))

  (defun my:titleized-at-point-or-region ()
    "Convert snaked, dashed, underscored, camelcase, or spaced words in region to Title Case."
    (interactive)
    (operate-on-point-or-region 's-titleized-words))

  (defun my:flush-blank-lines ()
    "Flush blank lines."
    (interactive)
    (flush-lines "^\s*$" nil nil t))

  (joe/set-leader-keys
   "bb" 'my:switch-to-blah-buffer
   "bB" 'my:new-blah-buffer
   "nb" 'my:nuke-all-buffers
   "ss" 'my:snake-case-at-point-or-region
   "sd" 'my:dasherise-at-point-or-region
   "scu" 'my:upper-camelcase-at-point-or-region
   "scl" 'my:lower-camelcase-at-point-or-region
   "sh" 'my:humanize-at-point-or-region
   "st" 'my:titleized-at-point-or-region
   "tm" 'my:toggle-mac-modifiers
   "xf" 'my:flush-blank-lines
   "xo" 'delete-blank-lines))

(defun my:insert-newline-and-follow ()
  (interactive)
  (insert "\n")
  (indent-according-to-mode))

(defun my:insert-newline-and-stay ()
  (interactive)
  (save-excursion (my:insert-newline-and-follow)))

(define-key evil-normal-state-map (kbd "[ RET")
  'my:insert-newline-and-stay)
(define-key evil-normal-state-map (kbd "] RET")
  'my:insert-newline-and-follow)

(evil-leader/set-key "iSr" 'yas-reload-all)

(defvar sandlot-jscomp-linter-executable
  "/google/data/ro/teams/devtools/glint/linters/live/Linter_deploy.jar"
  "Path to the jscomp linter.")

(defun sandlot-run-jscomp-lint-on-file ()
  (interactive)
  (compile (concat sandlot-jscomp-linter-executable " " (buffer-file-name))))

(defun sandlot-run-jscomp-lint-on-everything ()
  (interactive)
  (compile (concat "find ~/depot/google3/partnerservices/pds/sandlot "
                   "-name '*.js' -print0 "
                   "| xargs -0 "
                   sandlot-jscomp-linter-executable)))

(defun revbufs ()
  "Revert all buffers."
  (interactive)
  (let ((conflicts  '())
        (orphans    '())
        (reverts    '())
        (report-buf (get-buffer-create "*revbufs*")))

    ;; Process the buffers.
    (mapc (function
           (lambda (buf)
             (let ((file-name (buffer-file-name buf)))
               (cond
                ;; If buf is the report buf, ignore it.
                ((eq buf report-buf) nil)
                ;; If buf is not a file buf, ignore it.
                ((not file-name) nil)
                ;; If buf file doesn't exist, buf is an orphan.
                ((not (file-exists-p file-name))
                 (setq orphans (nconc orphans (list buf))))
                ;; If file modified since buf visit, buf is either a conflict
                ;; (if it's modified) or we should revert it.
                ((not (verify-visited-file-modtime buf))
                 (if (buffer-modified-p buf)
                     (setq conflicts (nconc conflicts (list buf)))
                   (with-current-buffer buf
                     (revert-buffer t t))
                   (setq reverts (nconc reverts (list buf)))))))))
          (copy-sequence (buffer-list)))

    ;; Prepare the report buffer.
    (with-current-buffer report-buf
      (setq buffer-read-only nil
            truncate-lines   t)
      (delete-region (point-min) (point-max))
      (insert (revbufs-format-list conflicts "CONFLICTS")
              (revbufs-format-list orphans   "ORPHANS")
              (revbufs-format-list reverts   "REVERTS"))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (bury-buffer report-buf)

    ;; Print message in echo area.
    (if (or conflicts orphans)
        (progn
          (display-buffer report-buf)
          (message
           (concat
            (format "Reverted %s with"
                    (revbufs-quantity (length reverts) "buffer"))
            (if conflicts
                (format " %s%s"
                        (revbufs-quantity (length conflicts) "conflict")
                        (if orphans " and" "")))
            (if orphans
                (format " %s"
                        (revbufs-quantity (length orphans) "orphan"))))))
      (if reverts
          (message "Reverted %s." (revbufs-quantity (length reverts) "buffer"))
        (message "No buffers need reverting.")))))

(defun revbufs-format-list (list label)
  "Format LIST of buffers with LABEL."
  (if list
      (concat label
              (format " (%s):\n" (length list))
              (mapconcat
               (function
                (lambda (buf)
                  (format "  %-20s %s\n"
                          (buffer-name buf)
                          (buffer-file-name buf))))
               list
               ""))
    ""))

(defun revbufs-quantity (num what)
  (format "%d %s%s" num what (if (= num 1) "" "s")))

(defun my:ffap-with-line ()
  (interactive)
  ;; TODO: handle case where point is positioned on the number.
  (my:ffap-with-line-from-string (ffap-string-at-point)))

(defun my:ffap-with-line-from-string (file-name-and-line)
  "Given a file path with a line number, go to the file at that line number.
FILE-NAME-AND-LINE is assumed to be of the form file/path.el:32.
If there is no line number, drop back to `find-file-at-point'."
  (message "my:ffap-with-line-from-string")
  (if (string-match "\\(.*\\):\\([0-9]+\\)" file-name-and-line)
      (let ((file-path (match-string 1 file-name-and-line))
            (line-number (match-string 2 file-name-and-line))
            ;; There's no way to get ffap to process just a string.
            ;; `ffap-file-at-point' insists on parsing the thing at point by
            ;; itself.  This is okay as long as `ffap-string-at-point' does what
            ;; we want.
            (full-path (ffap-file-at-point)))
        (find-file-other-window full-path)
        (and line-number (goto-line (string-to-number line-number))))
    (find-file-other-window (ffap-file-at-point))))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map "gf" #'my:ffap-with-line))

(defun my:copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(joe/set-leader-keys "yf" #'my:copy-file-name-to-clipboard)

;; Have a period invoke auto-fill in addition to space and newline.
(aset auto-fill-chars ?. t)

;; (with-eval-after-load 'dired

;; (when (eq system-type 'darwin)
;;   (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"))

;; (let ((long-listing "-l")
;;       (show-all "--all")
;;       (dont-show-group "--no-group")
;;       (human-readable "--human-readable")
;;       (natural-number-sorting "-v")
;;       (group-directories-first "--group-directories-first"))

;;   (setq dired-listing-switches
;;         (mapconcat 'identity (list long-listing
;;                                    show-all
;;                                    dont-show-group
;;                                    human-readable
;;                                    natural-number-sorting
;;                                    group-directories-first)
;;                    " "))))

;;; config.el ends here
