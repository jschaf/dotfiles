;;; config.el --- Configuration for Joe's private layer.


;;; Commentary:
;;

;;; Code:


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

;; (with-eval-after-load 'lisp-mode
;;   (require 'pp)
;;   (defun my-pp-eval-last-sexp-in-current-buffer ()
;;     (interactive)
;;     (pp-eval-last-sexp t))
;;   (spacemacs/set-leader-keys-for-major-mode 'lisp-interaction-mode
;;     "ep" 'my-pp-eval-last-sexp-in-current-buffer
;;     "eP" 'pp-eval-last-sexp))

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

(evil-leader/set-key "iSr" 'yas-reload-all)

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

(defun my:get-relative-name (file-name)
  "Gets the relative FILE-NAME from a projectile directory or $HOME.
If file-name isn't underneath a projectile root or $HOME return
the expanded, absolute FILE-NAME."
  (let* ((projectile-root (condition-case nil
                              (projectile-project-root)
                            (error nil)))
         (home-directory (expand-file-name "~/"))
         (full-file-name (expand-file-name file-name))
         (base-directory
          (or projectile-root
              (and (s-starts-with-p home-directory full-file-name)
                   home-directory)
              nil)))
    (if base-directory
        (file-relative-name full-file-name base-directory)
      full-file-name)))

(defun my:get-buffer-file-name ()
  "Gets a reasonable file name for different types of buffers.
If no reasonable file-name is possible, return empty string."
  (if (equal major-mode 'dired-mode)
      default-directory
    (or (buffer-file-name) "")))

(defun my:copy-file-name-relative-to-clipboard ()
  "Copy the current buffer file name to the clipboard.
Gets the file name relative to the projectile directory or $HOME
directory."
  (interactive)
  (message "%s" (kill-new (my:get-relative-name (my:get-buffer-file-name)))))

(defun my:copy-file-name-absolute-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (message "%s" (kill-new (my:get-buffer-file-name))))

(joe/set-leader-keys
 "yf" #'my:copy-file-name-relative-to-clipboard
 "yF" #'my:copy-file-name-absolute-to-clipboard
 )

(defvar my:org-to-html-convert-command
  (cond
   ((eq system-type 'darwin)
    (concat
     "osascript -e 'the clipboard as \"HTML\"' "
     "| perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' "
     "| pandoc -f html -t json "
     "| pandoc -f json -t org"
     ))
   ((eq system-type 'gnu/linux)
    (concat "NO_AT_BRIDGE=1; clipcli.py text/html "
            "| pandoc -f html -t json "
            "| pandoc -f json -t org")))
  "Shell command to convert HTML to org.")

(defun my:chomp-trailing-newlines (string)
  "Collapses trailing newlines to a single newline."
  ;; \\' is a special elisp regexp form for end of string.
  (replace-regexp-in-string "\n+\\'" "\n" string))

(defun my:replace-nbsp-with-space (string)
  "Replaces NBSP with regular spaces."
  (s-replace-all '((" " . " ")
                   (" " . " "))
                 string))

(defun my:remove-null-from-string (string)
  "Remove the null char \0 from a string."
  (s-replace " " "" string))

(defun my:paste-html-as-org ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new (s-trim-right
             (my:chomp-trailing-newlines
              (my:remove-null-from-string
               (my:replace-nbsp-with-space
                (shell-command-to-string my:org-to-html-convert-command))))))
  (yank))

(joe/set-leader-keys
 "ph" #'my:paste-html-as-org)

;; Have a period and question mark invoke auto-fill in addition to space and
;; newline.
(aset auto-fill-chars ?. t)
(aset auto-fill-chars ?? t)

(with-eval-after-load 'ediff
  (defun ediff-copy-both-to-C ()
    "Add region A and region B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(defvar pdfize-latex-template-location "~/.dotfiles/layers/joe/code-template.tex"
  "The location of the LaTeX template to use to conver source
  code to PDF.")

(defun pdfize-print-emacs-buffer (&optional buffer)
  "Print the current buffer by compiling it with LaTeX."
  ;; Get buffer or current buffer

  ;; Convert buffer to pdf
  )

(defun pdfize-open-buffer-as-pdf (&optional buffer)
  "Open BUFFER as a pretty-printed PDF."
  (interactive)
  (let* ((buffer (if buffer (get-buffer buffer) (current-buffer))))
    (org-open-file (pdfize-convert-buffer-to-pdf buffer))))

(defun pdfize-convert-buffer-to-pdf (&optional buffer)
  "Convert BUFFER into a PDF representation using a LaTeX template."
  (let* ((template (pdfize-get-latex-template-string))
         (buffer (if buffer
                     (get-buffer buffer)
                   (current-buffer)))
         (filled-template (pdfize-fill-template template buffer))
         (filled-template-path (pdfize-save-filled-template
                                filled-template buffer)))
    (pdfize-compile-file filled-template-path)))

(defvar pdfize-output-directory "/tmp/pdfize"
  "Output directory for rendered PDFs.")

(defun pdfize-ensure-output-directory-exists ()
  "Ensure that `pdfize-output-directory' exists."
  (make-directory pdfize-output-directory 'parents))

(defun pdfize-save-filled-template (filled-template buffer)
  "Write FILLED-TEMPLATE using the name of BUFFER.
Writes file to `pdfize-output-directory'.  If BUFFER doesn't have
a file-name, use a random number."
  (pdfize-ensure-output-directory-exists)
  (let* ((file-name-raw (pdfize-template-get-file-name buffer))
         (file-name (if (string-equal file-name-raw "")
                        (format "%d" (random 10000))
                      file-name-raw))
         (file-path (concat pdfize-output-directory "/" file-name ".tex")))
    (write-region filled-template nil file-path)
    file-path))

(defun pdfize-compile-file (file-name)
  "Given a FILE-NAME, compile it to OUTPUT-PATH and return the file path"

  ;; Compiling LaTeX is hard and is compounded by Emacs' lack of async
  ;; processing capabilities other than starting a process.  So, we cheat and
  ;; use org-mode.
  (let ((org-latex-pdf-process
         (list (concat "xelatex -shell-escape -interaction nonstopmode "
                       "--output-directory %o %f"))))
    (org-latex-compile file-name)))

(defun pdfize-get-latex-template-string ()
  "Return the LaTeX template as a string."
  (with-temp-buffer
    (insert-file-contents pdfize-latex-template-location)
    (buffer-string)))

(defvar pdfize-field-function-mapping
  '(("PDFIZEFILEPATH" . pdfize-template-get-file-path)
    ("PDFIZEFILENAME" . pdfize-template-get-file-name)
    ("PDFIZELANGUAGE" . pdfize-template-get-language)))

(defun pdfize-fill-template (template-string buffer)
  "Given a TEMPLATE-STRING, fill it using context from BUFFER."
  (pdfize-fill-template-from-buffer template-string
                                    pdfize-field-function-mapping
                                    buffer))

(defun pdfize-fill-template-from-buffer (template-string function-mapping buffer)
  "Given a TEMPLATE-STRING, use FUNCTION-MAPPING to fill the fields from BUFFER."
  (let ((template template-string))
    (cl-loop for (var-name . function) in function-mapping
             do
             (setq template (pdfize-fill-template-variable
                             template
                             var-name
                             (funcall function buffer))))
    template))

(defun pdfize-fill-template-variable (template-string var-name var-value)
  "In TEMPLATE-STRING, replace VAR-NAME with VAR-VALUE.

For example, with (pdfize-fill-template-variable below PDFIZEFILENAME FOOBAR.txt)
This:
\newcommand{\PDFIZEFILENAME}{}
would turn into
\newcommand{\PDFIZEFILENAME}{FOOBAR.txt}
"
  (with-temp-buffer
    (insert-string template-string)
    (goto-char (point-min))
    (when (search-forward var-name nil 'noerror)
      (search-forward "{")
      (insert var-value))
    (buffer-string)))


(defun pdfize-template-get-file-path (&optional buffer)
  "Given a BUFFER, get the full file path for minted.
If buffer is nil, use the current buffer.  If BUFFER does not
have a backing file, return the empty string"
  (let* ((buffer (if buffer
                     (get-buffer buffer)
                   (current-buffer)))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (file-truename file-name)
      ;; It might be a buffer without a backing file, like *scratch*.
      "")))

(defun pdfize-template-get-file-name (&optional buffer)
  "Given a BUFFER, return the file name."
  (file-name-nondirectory (pdfize-template-get-file-path buffer)))

(defun pdfize-template-get-language (&optional buffer)
  "Given a BUFFER, get the language name for minted.
If buffer is nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (pdfize-template-get-language-from-buffer buffer)))

(defun pdfize-template-get-language-from-buffer (buffer)
  "Given a BUFFER, return the language used for use in LaTeX minted.
Use `pdfize-major-mode-to-language-mapping' otherwise just remove
the \"-mode\" from the major mode in BUFFER"
  (with-current-buffer buffer
    (let* ((major-mode-name (symbol-name major-mode))
           (lang-mapping (assoc major-mode-name
                                pdfize-major-mode-to-language-mapping)))
      (if lang-mapping
          (cdr lang-mapping)
        (pdfize-get-language-from-major-mode major-mode-name)))))

(defun pdfize-get-language-from-major-mode (major-mode-name)
  "Given the name of MAJOR-MODE, return a string of the language.
If the string doesn't match the form .*-mode, return an empty
string."
  (let ((lang-name-match (string-match "\\(.*\\)-mode" major-mode-name)))
    (if lang-name-match
        (match-string 1 major-mode-name)
      "")))

(defvar pdfize-major-mode-to-language-mapping
  '(("js2-mode" . "javascript"))
  "Mapping used for major modes that don't map cleanly to a
  language name.")

(defun get-current-provide-string ()
  "Returns the first goog.provide() string in the current buffer,
or nil if not found."
  (save-excursion
    (beginning-of-buffer)
    (when (re-search-forward
           "goog\.provide *(*[\"']\\([A-Za-z0-9.]+\\)[\"'] *)" nil t)
      (match-string-no-properties 1))))


(defun ediff-copy-both-to-C ()
  "In an ediff buffer, copy both diffs from A and B into C."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference
                                               'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference
                                               'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "B" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun my:mac-get-current-url ()
  (let ((result
         (do-applescript
          "tell app \"Google Chrome\" to URL of active tab of window 1")))
    (replace-regexp-in-string
     "^\"\\|\"$" "" (car (split-string result "[\r\n]+" t)))))

(defun my:mac-get-current-tab-title ()
  (let ((result
         (do-applescript
          "tell app \"Google Chrome\" to title of active tab of window 1")))
    (replace-regexp-in-string
     "^\"\\|\"$" "" (car (split-string result "[\r\n]+" t)))))

(defun my:get-current-url ()
  (pcase system-type
    ((pred (string= "gnu/linux")))
    ((pred (string= "darwin")) (my:mac-get-current-url))))

(defun my:get-current-tab-title ()
  (pcase system-type
    ((pred (string= "gnu/linux")))
    ((pred (string= "darwin")) (my:mac-get-current-tab-title))))

(defun my:insert-current-url ()
  (interactive)
  (insert (my:get-current-url)))

(defun my:insert-current-url-org-link ()
  (interactive)
  (insert (format "[[%s][%s]]" (my:get-current-url)
                  (my:get-current-tab-title))))

(defun my:time-duration-to-seconds (time-string)
  "Parses a string like \"2m 17s\" in to seconds."
  (pcase (s-match (concat "\\(\\([0-9]+\\)m\\)?"
                          " ?"
                          "\\(\\([0-9]+\\)s\\)?")
                  (s-trim time-string))
    ;; Only minutes
    (`(,whole-string ,minutes ,minutes-number)
     (+ (* 60 (string-to-number minutes-number))))

    (`(,whole-string ,minutes ,minutes-number ,seconds ,seconds-number)
     (+ (* 60 (string-to-number (or minutes-number "0")))
        (string-to-number (or seconds-number "0"))))

    (_ (error "Failed to parse time duration: %s" time-string))))

(defun my:configure-emacs-for-sshfs ()
  "Disables git and VC features.
This is mainly useful for SSHFS."
  (interactive)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (global-git-gutter-mode -1)
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
  (when (equal major-mode 'python-mode)
    (message "Disabling python mode stuff.")
    (anaconda-mode -1)
    (eldoc-mode -1)))

(defun my:is-tty ()
  ;; There's not a good way to distinguish between terminal emulators and
  ;; framebuffer terminals.  We'll use SSH as a heuristic.
  (not (or window-system (getenv "DISPLAY") (getenv "SSH_CONNECTION"))))

(unless window-system
  (defun my:paste-from-emacs-client ()
    "Paste into a terminal Emacs."
    (if window-system
        (error "Trying to paste into GUI emacs.")
      (let ((paste-data (s-trim (shell-command-to-string "clipboard-paste"))))
        ;; When running via emacsclient, paste into the current buffer.  Without
        ;; this, we would paste into the server buffer.
        (with-current-buffer (window-buffer)
          (insert paste-data))
        ;; Add to kill-ring
        (kill-new paste-data))))

  (defun my:copy-from-terminal (text)
    "Store text in the clipboard from a terminal Emacs."
    (if window-system
        (error "Trying to copy text in GUI emacs.")
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "~/bin/clipboard-copy"))))

  (defun my:buffer-substring-terminal-filter (beg end &optional delete)
    "A filter that uses the default filter but also adds text to clipboard."
    (let ((result (buffer-substring--filter beg end delete)))
      ;; Only copy sizable entries to avoid unnecessary system calls.
      (when (> (length result) 4)
        (my:copy-from-terminal result))
      result))

  (setq-default filter-buffer-substring-function
                #'my:buffer-substring-terminal-filter))

;; LaTeX template
;; Minted - http://www.ctan.org/pkg/minted

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
