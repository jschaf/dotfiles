;;; debug-hooks.el --- Debug all the hooks -*- lexical-binding: t; -*-

(defvar debug-hooks-buffer "*debug-hooks*"
  "The buffer to output hook debug information.")

(defvar debug-hooks--current-hook nil
  "The current hook that's running.")

;; Output

;; Most Recent at top.

;; Command: self-insert-command
;; At: %t:%6n
;; 100ms post-command-hooks
;;    25ms pch-1
;;    25ms pch-1
;; find-file-modification-hooks
;;    33ms ffmh

(defvar debug-hooks-all-hooks
  '(
    ;; activate-mark-hook
    ;; deactivate-mark-hook
    ;; after-change-functions
    ;; before-change-functions
    ;; first-change-hook
    ;; after-change-major-mode-hook
    ;; change-major-mode-after-body-hook
    ;; after-init-hook
    ;; before-init-hook
    ;; emacs-startup-hook
    ;; window-setup-hook
    ;; after-insert-file-functions
    ;; write-region-annotate-functions
    ;; write-region-post-annotation-function
    ;; after-make-frame-functions
    ;; before-make-frame-hook
    ;; after-save-hook
    ;; before-save-hook
    ;; write-contents-functions
    ;; write-file-functions
    ;; after-setting-font-hook
    ;; auto-save-hook
    ;; before-hack-local-variables-hook
    ;; hack-local-variables-hook
    ;; buffer-access-fontify-functions
    ;; buffer-list-update-hook
    ;; buffer-quit-function
    ;; change-major-mode-hook
    ;; command-line-functions
    ;; delayed-warnings-hook
    ;; focus-in-hook
    ;; focus-out-hook
    ;; delete-frame-functions
    ;; delete-terminal-functions
    ;; pop-up-frame-function
    ;; split-window-preferred-function
    ;; echo-area-clear-hook
    ;; find-file-hook
    ;; find-file-not-found-functions
    ;; font-lock-extend-after-change-region-function
    ;; font-lock-extend-region-functions
    ;; font-lock-fontify-buffer-function
    ;; font-lock-fontify-region-function
    ;; font-lock-mark-block-function
    ;; font-lock-unfontify-buffer-function
    ;; font-lock-unfontify-region-function
    ;; fontification-functions
    ;; frame-auto-hide-function
    ;; kill-buffer-hook
    ;; kill-buffer-query-functions
    ;; kill-emacs-hook
    ;; kill-emacs-query-functions
    ;; menu-bar-update-hook
    ;; minibuffer-setup-hook
    ;; minibuffer-exit-hook
    ;; mouse-leave-buffer-hook
    ;; mouse-position-function
    ;; prefix-command-echo-keystrokes-functions
    ;; prefix-command-preserve-state-hook
    ;; pre-redisplay-functions
    post-command-hook
    ;; pre-command-hook
    ;; post-gc-hook
    ;; post-self-insert-hook
    ;; suspend-hook
    ;; suspend-resume-hook
    ;; suspend-tty-functions
    ;; resume-tty-functions
    ;; syntax-begin-function
    ;; syntax-propertize-extend-region-functions
    ;; syntax-propertize-function
    ;; font-lock-syntactic-face-function
    ;; temp-buffer-setup-hook
    ;; temp-buffer-show-function
    ;; temp-buffer-show-hook
    ;; tty-setup-hook
    ;; window-configuration-change-hook
    ;; window-scroll-functions
    ;; window-size-change-functions
    ;; window-text-change-functions
    )
  )

(define-minor-mode debug-hooks-mode
  "Toggle `debug-hooks-mode' on and off."
  :lighter " debug-hooks"
  :init-value nil
  (cond
   ;; Disable
   (debug-hooks-mode
    (debug-hooks-unadvise-run-hooks)
    (debug-hooks-unadvise-hooks debug-hooks-all-hooks))

   ;; Enable
   (t
    (debug-hooks-advise-run-hooks)
    (debug-hooks-advise-hooks debug-hooks-all-hooks))))

(defun hooks//log-hook-message (message)
  (let ((inhibit-modification-hooks t))
    (with-current-buffer "hooks"
      (goto-char (point-min))
      (when (> (buffer-size) 1000)
        (erase-buffer))
      (insert (concat
               (format-time-string "%M:%S.%5N")
               ":   "
               message))
      (insert "\n"))))

(defun debug-hooks-stopwatch-start ()
  "Start a stop-watch and return the current time.
The time format matches `current-time'."
  (current-time))

(defun debug-hooks-stopwatch-stop-in-millis (start-time)
  "Return the number of milliseconds elapsed since START-TIME.
START-TIME is the normal elisp representation of an instant in
time.  The representation is a 4 element list of integers.  See
the `current-time' documentation for details."
  (* (time-to-seconds
      (time-subtract (current-time) start-time))
     1000))

(defun debug-hooks-advise-hooks (hooks)
  "Advise all HOOKS to log debug output."
  (cl-loop for hook in hooks
           do
           (cond
            ((and (boundp hook) (listp (symbol-value hook)))
             (cl-loop for function in hook do
                      (debug-hooks-advise-single-function function hook)))
            ((functionp hook)
             (debug-hooks-advise-single-function hook 'self)))))

(defun debug-hooks-advise-single-function (wrapped-fn parent-hook)
  "Advise a single WRAPPED-FN to log debug output.
PARENT-HOOK is hook that called WRAPPED-FN and is used for
debuggin purposes."
  (let ((advisor-symbol (debug-hooks--make-advisor-name wrapped-fn parent-hook))
        (advisor-fn
         (debug-hooks--make-function-advisor wrapped-fn parent-hook)))
    (eval advisor-fn)
    (advice-add wrapped-fn :around advisor-symbol)))

(defun debug-hooks--make-function-advisor (wrapped-fn parent-hook)
  "Create a function to advise WRAPPED-FN.
PARENT-HOOK is the hook that triggered WRAPPED-FN.  This function
is necessary because when advising a function, the advisor
doesn't have access to the function name and there's not a way to
parameterize advice."
  (let ((advisor-symbol
         (debug-hooks--make-advisor-name wrapped-fn parent-hook)))
    `(defun ,advisor-symbol (orig-fn &rest args)
       "Advise around ORIG-FN write debug output."
       (let* ((start-time (debug-hooks-stopwatch-start))
              (result (apply orig-fn args))
              (latency (debug-hooks-stopwatch-stop-in-millis start-time)))
         (when (eq debug-hooks--current-hook (quote ,parent-hook))
           (debug-hooks-write-log-info
            (quote ,wrapped-fn) (quote ,parent-hook) latency))
         result))))

(defun debug-hooks--make-advisor-name (wrapped-fn parent-hook)
  "Make a name for advice of WRAPPED-FN."
  (intern (format "debug-hooks--wrap-%s-%s" wrapped-fn parent-hook)))


;; (debug-hooks-advise-single-function
;;  'global-undo-tree-mode-check-buffers 'post-command-hook)

;; (debug-hooks--make-function-advisor
;;  'global-undo-tree-mode-check-buffers 'post-command-hook)

;; (let (
;;       (function-name )
;;       (advisor
;;        (eval (debug-hooks--make-function-advisor
;;               'global-undo-tree-mode-check-buffers
;;               'post-command-hook))))
;;   (advice-add 'global-undo-tree-mode-check-buffers
;;               :around advisor))


;; (setq my-hook '())

;; (defun my-fun ())

;; (add-to-list 'my-hook 'my-fun)


;; (debug-hooks--make-function-advisor 'my-fun 'my-hook)

(defun debug-hooks-log-hook-info (orig-fn &rest args)
  "Record debug info of ORIG-FN called with ARGS."
  (let* ((start-time (debug-hooks-stopwatch-start))
         (result (apply orig-fn args))
         (latency (debug-hooks-stopwatch-stop-in-millis start-time)))
    (debug-hooks-write-log-info orig-fn latency)
    result))

(defun debug-hooks-write-log-info (func parent-hook latency)
  "Write log info."
  (with-current-buffer (get-buffer-create debug-hooks-buffer)
    (when (> (buffer-size) 2000)
      (erase-buffer))

    (goto-char (point-min))
    (insert (format "%.3f ms:  %s - %s\n"
                    latency
                    parent-hook
                    (symbol-name func)))))

(defun debug-hooks--run-hooks-update-current-hook (orig-fn &rest args)
  "Update the `debug-hooks--current-hook' variable"
  (setq debug-hooks--current-hook (car-safe args)))

(defun debug-hooks-advise-run-hooks ()
  "Add advice to `run-hooks' to record the current hook."
  (advice-add 'run-hooks :before #'debug-hooks--run-hooks-update-current-hook))

(defun debug-hooks-unadvise-run-hooks ()
  "Remove advice to `run-hooks' to record the current hook."
  (advice-remove 'run-hooks #'debug-hooks--run-hooks-update-current-hook))

(defun debug-hooks-unadvise-hooks (hooks)
  "Remove all advice from HOOKS."
  ;; if single function remove advice directly
  ;; otherwise remove advice from all members
  nil)

(defun debug-hooks-unadvise-single-hook (hook)
  "Remove advice from HOOK.")

