; miscelaneous useful functions to supplement standard gnumacs

(defun other-window-backwards (&optional arg)
  "Other-window in reverse order."
  (interactive "P")
  (if (null arg)
      (setq arg 1))
  (other-window (- arg)))

(defun new-kill-line (&optional arg)
  "Kill the rest of the current line; before a newline, kill the newline.
If at the beginning of the line, kill entire line."
  (interactive "P")
  (if (and (bolp) (null arg))
      (setq arg 1))
  (kill-line arg))

(defun new-undo (&optional arg &aux c)
  "Undo changes until the user gets tired."
  (interactive)
  (undo)
  (while (progn (message "Hit SPACE to undo more")
		(= (setq c (read-char)) ?\ ))
    (undo-boundary)
    (undo-more 1))
  (message "Finished undoing.")
  (if (and (not (= c ?\n)) (not (= c ?\r)))
      (setq unread-command-events c)))

(defun indent-region-relative (n)
  "Indent region by N columns, relative to the current position.
   N may be negative."
  (interactive "nIndent region: ")
  (save-excursion
    (if (< (mark) (point)) (exchange-point-and-mark))
    (while (and (<= (point) (mark)) (not (eobp)))
      (beginning-of-line)
      (let ((ci (current-indentation)))
	(delete-horizontal-space)
	(indent-to (+ ci n))
	(end-of-line)
	(next-line 1)
	(if (not (eobp)) (beginning-of-line))))))

(defun goto-matching-sexp (&optional count)
  "Go to the matching parenthesis, like % in vi."
  (interactive "p")
  (if (or (null count) (= count 0))
      (setq count 1))
  (cond ((memq (following-char) '(?( ?[ ?{))	; )
	 (forward-sexp count))
	(t
	 (backward-sexp count))))

(defun widen-window (count)
  "Make horizontally split window wider."
  (interactive "p")
  (enlarge-window count t))

(defun beginning-of-window ()
  "Move the cursor to the first character in the window"
  (interactive)
  (move-to-window-line 0))

(defun end-of-window ()
  "Move the cursor to the last character in the window"
  (interactive)
  (move-to-window-line -1))

(defun new-beginning-of-buffer ()
  "Move cursor to the beginning of the current buffer."
  (interactive)
  (goto-char (point-min)))

(defun new-end-of-buffer ()
  "Move cursor to the end of the current buffer."
  (interactive)
  (goto-char (point-max)))

(defun swap-chars ()
  "Transpose the two previous characters, Gosling style."
  (interactive)
  (backward-char)
  (transpose-chars 1))

(defun save-modified-buffers (&optional arg)
  "Save all modified file-visiting buffers without asking unless an argument
is given.  If one is, ask user about each buffer before saving."
  (interactive "P")
  (let (considered (list (buffer-list)))
    (while list
      (let ((buffer (car list)))
	(condition-case ()
	    (and (buffer-modified-p buffer)
		 (buffer-file-name buffer)
		 (setq considered t)
		 (or (not arg)
		     (y-or-n-p (format "Save file %s? " (buffer-file-name buffer))))
		 (save-excursion
		   (set-buffer buffer)
		   (save-buffer)))
	  (error nil)))
      (setq list (cdr list)))
    (and save-abbrevs abbrevs-changed
	 (setq considered t)
	 (or (not arg)
	     (y-or-n-p (format "Save abbrevs in %s? " abbrev-file-name)))
	 (progn
	  (write-abbrev-file nil)
	  (setq abbrevs-changed nil)))
    (if (not considered) (message "(No files need saving)"))))

(defun kill-file-buffers (&optional arg)
  "Kill all file-visiting buffers without confirmation
unless a prefix argument is given."
  (interactive "P")
  (let ((considered nil) (buffer nil) (list (buffer-list)))
    (while list
      (setq buffer (car list))
      (if (buffer-file-name buffer)
	  (progn
	    (setq considered t)
	    (if (or (not arg)
		    (y-or-n-p (format "Kill buffer %s? "
				      (buffer-name buffer))))
		(kill-buffer buffer))))
      (setq list (cdr list)))
    (if (not considered) (message "(No file buffers to kill)"))))

(defun revert-file-buffers (&optional arg)
  "Revert all file-visiting buffers without confirmation
unless a prefix argument is given."
  (interactive "P")
  (save-excursion
    (let ((considered nil) (buffer nil) (list (buffer-list)))
      (while list
	(setq buffer (car list))
	(if (buffer-file-name buffer)
	    (progn
	      (setq considered t)
	      (if (or (not arg)
		      (y-or-n-p (format "Revert buffer %s? "
					(buffer-name buffer))))
		  (progn
		    (switch-to-buffer buffer)
		    (revert-buffer t t)))))
	(setq list (cdr list)))
      (if (not considered) (message "(No file buffers to kill)")))))

(defun exit-emacs ()
  "An easy name for save-buffers-kill-emacs."
  (interactive)
  (save-buffers-kill-emacs))

(defun exit-emacs-maybe ()
  "Confirm exit then call save-buffers-kill-emacs."
  (interactive)
  (if (y-or-n-p "You mean actually exit emacs? ")
      (save-buffers-kill-emacs)
      (message "")))

(defun yow-a-lot ()
  (interactive)
  (yow nil t)
  (while (y-or-n-p "More Zippy wisdom? ")
    (call-interactively (function yow))
    (sit-for 5)))

(defun setenv (name val)
  "Set environment variable VAR to VAL for child processes."
  (interactive "sVariable: \nsValue: ")
  (let* ((rest process-environment)
	 (regex (concat "^" name "="))
	 (match (string-match regex (car rest)))
	 (value (concat name "=" val)))
    (while (and (null match) (not (setq rest (cdr rest))))
     (setq match (string-match regex (car rest))))
    (if match
	(rplaca rest value)
      (setq process-environment (cons value process-environment)))))

(defun unsetenv (name)
  "Remove environment variable VAR for child processes."
  (interactive "sVariable: ")
  (let* ((rest process-environment)
	 (regex (concat "^" name "="))
	 (last nil)
	 (match (string-match regex (car rest))))
    (while (and (null match) (not (setq rest (cdr rest))))
     (setq match (string-match regex (car rest)))
     (setq last rest))
    (if (not match)
	nil
      (if (null last)
	  (setq process-environment (cdr process-environment))
	(rplacd last (cdr rest))))))

(defun really-revert-buffer ()
  "Revert the buffer without trying to use an auto save file or confirming."
  (interactive)
  (revert-buffer t t))
  
(defun webster-look ()
  "Look up the word's definition using the webster service."
  (interactive)
  (let (start end str str2 (buf (get-buffer-create "*Webster*")))
    (save-excursion
      (if (not (looking-at "\\<"))
	  (forward-word -1))
      (setq start (point))
      (forward-word 1)
      (setq end (point))
      (setq str (buffer-substring start end))
      (setq str (downcase str))
      (setq str2 (read-buffer (format "lookup word (default %s): " str)))
      (if str2 ( setq str str2) )
      (set-buffer buf)
      (widen)
      (erase-buffer)
      (call-process "webster" nil buf nil str )
      (mark-page)
      (replace-string "\n" " ")
      (display-buffer buf))))

(defun delete-window-or-frame ()
  "Delete the current window or the current frame if only one window."
  (interactive)
  (if (one-window-p)
      (delete-frame)
    (delete-window)))

(defun delete-and-bury-window ()
  "Delete the window and bury its buffer (move to end of list)."
  (interactive)
  (bury-buffer)
  (delete-window-or-frame))

(defun toggle-truncate-lines ()
  "Toggle the state of truncate-lines for the current buffer."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (redraw-display))

(defvar *saved-search-pattern* ""
  "Previous search pattern for repeating searches.")

(defun new-search-forward (pattern)
  "Search forward, remembering previous pattern and repeating it if no new pattern."
  (interactive "sSearch forward: ")
  (if (equal pattern "")
      (setq pattern *saved-search-pattern*)
      (setq *saved-search-pattern* pattern))
  (search-forward-regexp pattern))

(defun new-search-backward (pattern)
  "Search backward, remembering previous pattern and repeating it if no new pattern."
  (interactive "sSearch backward: ")
  (if (equal pattern "")
      (setq pattern *saved-search-pattern*)
      (setq *saved-search-pattern* pattern))
  (search-backward-regexp pattern))

(defun change-default-font ()
  "Prompt for a new font and change the default font to it."
  (interactive)
  (set-default-font (win32-select-font)))
