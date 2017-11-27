;;; abn-funcs-ivy.el --- Functions for ivy


;;; Commentary:
;;

;;; Code:

(defun abn/ivy-evil-registers ()
  "Show evil registers."
  (interactive)
  (let ((ivy-height 24))
    (ivy-read "Evil Registers:"
              (cl-loop for (key . val) in (evil-register-list)
                       collect (eval `(format "%s : %s" (propertize ,(char-to-string key) 'face 'font-lock-builtin-face)
                                              ,(or (and val
                                                        (stringp val)
                                                        (replace-regexp-in-string "\n" "^J" val))
                                                   ""))))
              :action #'abn/ivy-insert-evil-register)))

(defun abn/ivy-insert-evil-register (candidate)
  (insert (replace-regexp-in-string "\\^J" "\n"
                                    (substring-no-properties candidate 4))))

(defun abn//read-zsh-history (&rest args)
  ;; The reason for nbutlast:
  ;;
  ;; The last entry from my_get_history.sh is an empty string which
  ;; causes ivy to position the cursor at the end of the list rather
  ;; than beginning. See:
  ;; https://github.com/abo-abo/swiper/issues/1230
  (nbutlast (split-string (shell-command-to-string "my_get_history.sh") "\n")
            1))

(defun abn/counsel-zsh-history ()
  "Select entries from the ZSH history."
  (interactive)
  (ivy-read "ZSH history: "
            #'abn//read-zsh-history
            :keymap ivy-minibuffer-map
            :action #'insert
            :caller #'abn/counsel-zsh-history))

(defun abn//list-words-from-all-tmux-sessions (&rest args)
  "List words from all tmux sessions."
  (nbutlast
   (split-string
    (shell-command-to-string "tmux-completion -l '-a' -c '-S -2000'")
    "\n")))

(defun abn/counsel-tmux-words ()
  "Select words from all tmux sessions."
  (interactive)
  (ivy-read "tmux words: "
            #'abn//list-words-from-all-tmux-sessions
            :keymap ivy-minibuffer-map
            :action #'insert
            :caller #'abn//list-words-from-all-tmux-sessions))

(provide 'abn-funcs-ivy)

;;; abn-funcs-ivy.el ends here
