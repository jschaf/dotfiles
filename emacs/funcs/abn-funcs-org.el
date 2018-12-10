;;; abn-funcs-org.el --- Functions for org

;;; Commentary:
;;

;;; Code:

(defun abn/org-set-tag-as-drill ()
  "Set the current headline as a drill tag."
  (interactive)
  (org-toggle-tag "drill"))

(defun abn/org-drill-create-template ()
  "Insert a snippet for a new drill item."
  (interactive)
  (insert "*** Item                                      :drill:\n\n")
  (insert "Question\n\n")
  (insert "**** Answer\n\n")
  (insert "Answer\n")
  (search-backward "Item")
  (forward-word)
  (forward-char))

(defun abn/org-drill-create-template-cloze ()
  "Insert a template for cloze."
  (interactive)
  (insert "*** Item                                      :drill:\n")
  (insert ":PROPERTIES:\n:DRILL_CARD_TYPE: hide1cloze\n:END:\n\n")
  (insert "[Question] and [Answer]\n\n")
  (search-backward "Item")
  (forward-word)
  (forward-char))

(defun abn//org-yank-clipboard-html-linux ()

  )

(defun abn/org-yank-clipboard-html ()
  "Convert clipboard content from HTML to Org and then paste (yank)."
  (interactive)
  (kill-new ))

(provide 'abn-funcs-org)
;;; abn-funcs-org.el ends here
