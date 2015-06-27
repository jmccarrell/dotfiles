(defun c-comment-region (start end)
  "Comment out a region of C code using /*** and ***/ each on a line."
  (interactive "r")
  (goto-char start)
  (insert "/***\n")
  (goto-char (+ end 5))
  (insert "***/\n")
)

(defun c-ifdef-region (start end)
  "Comment out a region of C code using #if 0."
  (interactive "r")
  (goto-char start)
  (insert "#if 0\n")
  (goto-char (+ end 6))
  (insert "#endif\n")
)
