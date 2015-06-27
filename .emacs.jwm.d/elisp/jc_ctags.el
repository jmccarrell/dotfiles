;; tags package using the ctags++ format tags file
;;
;; it parses the output of ctags or ctags++ and implelemts the vi-like
;; tags functions ^] (visit-tag-at-point) and ^^ (return-from-tag) and
;; also a version which prompts from the minibuffer (visit-tag).
;;
;; Copyright (c) 1991, John Coker
;;

(defvar ctags-file-name "tags"
  "File name in current dirctory containing tags listing (as producted by
the ctags or ctags++ program).")

(defvar ctags-database nil
  "List of ctags from ctags file (this is actually a list of lists
of the tag name, the file name, and the search pattern.")

(defvar ctags-buffer "*Tags*"
  "Buffer in which to collect ctags file contents")

(defvar ctags-file-column 40
  "Column to which file names are tabbed in tags display buffer.")

(defvar ctags-visit-stack nil
  "List of markers in tag visiting stack.")

(defun load-tags-file (filename)
  "Read in a ctags file (\"./ctags\" is the default) to remake the
internal ctags list-of-lists for ctags searching functions."
  (interactive "fTags file: ")
  (if (not (stringp filename))
      (setq filename (expand-file-name ctags-file-name)))
  (if (not (file-exists-p filename))
      (error (concat "No ctags file \"" filename "\" in this directory!")))
  (message (concat "Loading ctags file \"" ctags-file-name "\" ..."))
  (setq ctags-database nil)
  (let ((bold (buffer-name))
	(bcur nil))
    (switch-to-buffer ctags-buffer t)
    (find-alternate-file filename)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((beg 0)
	    (sym nil)
	    (file nil)
	    (pat nil))
	(setq beg (point))
	(search-forward "\t")
	(backward-char)
	(setq sym (buffer-substring beg (point)))
	(forward-char)
	(setq beg (point))
	(search-forward "\t")
	(backward-char)
	(setq file (expand-file-name (buffer-substring beg (point))))
	(search-forward "/^")
	(setq beg (point))
	(search-forward "$/")
	(backward-char 2)
	(setq pat (buffer-substring beg (point)))
	(setq ctags-database (cons (list sym file pat) ctags-database)))
      (forward-line))
    (setq bcur (current-buffer))
    (switch-to-buffer bold)
    (message (concat "Loading ctags file \"" ctags-file-name "\" ... done."))
    (kill-buffer bcur)))

(defun ctags-find-decl (name)
  "Find the declaration of a function or variable definition through
the ctags ``database'' already read in (by load-tags-file)."
  (if (null ctags-database)
      (load-tags-file nil))
  (let ((rest ctags-database)
	(rname (concat "^.*::" name "$"))
	(found nil))
    (while rest
      (if (or (equal (car (car rest)) name)
	      (string-match rname (car (car rest))))
	  (setq found (cons (car rest) found)))
      (setq rest (cdr rest)))
    (cond ((<= (length found) 0)
	   (error "Tag \"%s\" not found." name))
	  ((= (length found) 1)
	   (car found))
	  (t
	   (let ((new-name nil))
	     (setq new-name (completing-read (format
					  "Found %d matches; qualified name: "
					             (length found))
					     found nil t nil))
	     (ctags-find-decl new-name))))))

(defun visit-tag (&optional name)
  "Find TAG in the ctags database and visit it's definition.
If interactive, prompt for tag name."
  (interactive "sTag name: ")
  (let ((found (ctags-find-decl name))
	(buf nil))
    (setq buf (get-file-buffer (car (cdr found))))
    (setq ctags-visit-stack (cons (point-marker) ctags-visit-stack))
    (if buf
	(switch-to-buffer buf)
      (find-file (car (cdr found))))
    (set-window-point (selected-window) (point-min))
    (search-forward (car (cdr (cdr found))))
    (beginning-of-line)))

(defun visit-tag-at-point (&optional prompt)
  "Read the C identifier word after point and visit its definition."
  (interactive "p")
  (if (and (numberp prompt) (> prompt 1))
      (let ((tag (read-from-minibuffer "Tag name: ")))
	(visit-tag tag))
    (let ((beg 0)
	  (name nil))
      (save-excursion
	(while (and (> (point) (point-min))
		    (or (= (char-syntax (preceding-char)) ?w)
			(= (preceding-char) ?:)
			(= (preceding-char) ?_)))
	  (backward-char))
	(setq beg (point))
	(while (and (< (point) (point-max))
		    (or (= (char-syntax (following-char)) ?w)
			(= (following-char) ?:)
			(= (following-char) ?_)))
	  (forward-char))
	(if (= beg (point))
	    (error "Point not in a word for the tag name."))
	(setq name (buffer-substring beg (point))))
    (visit-tag name))))

(defun return-from-tag ()
  "Return from the most recent tag visited with visit-tag."
  (interactive)
  (if (not ctags-visit-stack)
      (error "No more tag-visits to return from."))
  (let ((at (car ctags-visit-stack)))
    (setq ctags-visit-stack (cdr ctags-visit-stack))
    (switch-to-buffer (marker-buffer at))
    (set-window-point (selected-window) (marker-position at))))
