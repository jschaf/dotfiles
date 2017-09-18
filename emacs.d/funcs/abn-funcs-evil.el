;;; abn-funcs-evil.el --- Functions for evil

;;; Commentary:
;;

;;; Code:

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

(defun abn/shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun abn/shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))


(provide 'abn-funcs-evil)
;;; abn-funcs-evil.el ends here
