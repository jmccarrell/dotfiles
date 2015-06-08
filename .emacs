;;; jwms emacs startup file
;;;

;; Prepend my directories to the load path
(setq load-path
      (append (list (expand-file-name "~/src/emacs")) load-path))

;; add in the brew emacs directory when it exists
(cond ((file-exists-p "/usr/local/share/emacs/site-lisp")
       (setq load-path (append load-path (list "/usr/local/share/emacs/site-lisp")))))


;; set up my customization file
;; this is cut-and-paste from the emacs info node "Saving Customizations"
(cond ((< emacs-major-version 22)
       ;; Emacs 21 or less customization.
       (setq custom-file "~/.emacs-custom-21.el"))
      ((= emacs-major-version 22)
       (setq custom-file "~/.emacs-custom-22.el"))
      ((= emacs-major-version 23)
       (setq custom-file "~/.emacs-custom-23.el"))
      ((= emacs-major-version 24)
       (setq custom-file "~/.emacs-custom-24.el"))
      (t
       ;; past Emacs 23
       (setq custom-file "~/.emacs-custom-future.el")))

(load custom-file)

(setq user-full-name "Jeff McCarrell")
(setq user-mail-address "jeff@mccarrell.org")

;; always show syntax highlighting
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; reveal trailing whitespace
;; I am using the default; there is a fancier mechanism here:
;;   http://www.emacswiki.org/emacs/whitespace.el
(when (>= emacs-major-version 22)
  (setq-default show-trailing-whitespace t))

;; always end a file with a newline
(setq require-final-newline t)

;; get rid of all of the backup files
(setq backup-before-writing nil)
(setq make-backup-files nil)

(setq inhibit-startup-message t)
;; needed for emacs23
(setq inhibit-splash-screen t)

;; complete things without hesitation
(setq completion-auto-help nil)
(setq completion-ignore-case t)
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)
(define-key minibuffer-local-must-match-map " " 'minibuffer-complete)

;; kill the whole line when at the beginning of it
(setq kill-whole-line t)

;; use the gnome/X cut buffers for killing and yanking
(setq x-select-enable-clipboard t)

;; enable some more advanced commands
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; on OS X, set binding for meta to be command key, next to space bar
(when (eq 'darwin system-type)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;;; set up some key bindings
(define-key global-map "\C-xy" 'revert-buffer)
(define-key global-map "\C-c\C-c" 'comment-region)
(define-key global-map "\C-x\C-e" 'compile)
(define-key global-map "\e\C-g" 'goto-line)
;; (define-key global-map "\C-x\C-z" 'shell)
;; (define-key global-map "\C-z" 'shell)

;;; Setup up window preferences
(setq default-frame-alist
      (list '(menu-bar-lines . 1)
	    '(tool-bar-lines . 0)
	    '(vertical-scroll-bars . nil)
	    '(width . 100)
	    '(height . 52)
	    '(foreground-color . "white")
	    '(background-color . "black")
	    '(cursor-color . "DarkOrange")
	    ))
;;; (add-to-list 'default-frame-alist '(font . "lucidasanstypewriter-14"))
(if (and (>= emacs-major-version 23) (eq 'darwin system-type))
    (add-to-list 'default-frame-alist '(font . "Monaco-12"))
  (add-to-list 'default-frame-alist '(font . "9x15")))

(setq frame-title-format '("emacs: " buffer-file-name))
;; (set-language-environment "English")
(set-language-environment "UTF-8")

;; set up my faces
(when (>= emacs-major-version 21)
  (set-face-foreground 'font-lock-builtin-face "Peru")
  (set-face-foreground 'font-lock-comment-face "DeepSkyBlue1")
  (set-face-foreground 'font-lock-comment-delimiter-face "RoyalBlue2")
  (set-face-foreground 'font-lock-constant-face  "DarkGoldenrod1")
;; inherited from font-lock-string-face  (set-face-foreground 'font-lock-doc-face "")
  (set-face-foreground 'font-lock-function-name-face "Gold")
  (set-face-foreground 'font-lock-keyword-face "Orchid")
  (set-face-foreground 'font-lock-preprocessor-face "Tan")
  (set-face-foreground 'font-lock-string-face "LightCoral")
  (set-face-foreground 'font-lock-type-face "PaleGreen")
  (set-face-foreground 'font-lock-variable-name-face "LightSalmon1")
  (set-face-foreground 'font-lock-warning-face "Seashell")
  (set-face-background 'font-lock-warning-face "Tomato")
)


;;(setq truncate-partial-width-windows t)
;; (setq-default case-fold-search nil)
(setq split-height-threshold 14)
;; solve some duplicate buffers on filesystems that are case insensitive
(setq find-file-existing-other-name t)
(setq compilation-window-height 20)
(setq compilation-ask-about-save nil)
(setq compilation-read-command nil)
(setq auto-save-list-file-prefix
      (expand-file-name "~/tmp/emacs/emacs-saves"))

;;; hack function to replace an "alist" value
(defun alist-put (list prop val)
  (if (null list)
      ()
    (if (eq (car (car list)) prop)
	(rplacd (car list) val)
      (alist-put (cdr list) prop val))))

(add-hook 'c-mode-hook
      (function
       (lambda ()
	 (defconst indent-tabs-mode nil)
	 (defconst c-basic-offset 4)
	 (defconst c-argdecl-indent 4)
	 (alist-put c-offsets-alist 'substatement '+)
	 (alist-put c-offsets-alist 'substatement-open 0)
	 (defconst c-auto-newline nil)
	 (defconst c-tab-always-indent 1)
	 (defconst comment-start "/*\n *  ")
	 (defconst comment-end "\n */")
	 (defconst comment-start-skip "/\\*+\n \\*+")
	 (defconst comment-multi-line t)
	 (define-key c-mode-base-map "\r" 'newline-and-indent)
	 (define-key c-mode-base-map "\n" 'newline)
;;;(define-key c-mode-base-map "\t" 'tab-to-tab-stop)
;;;(define-key c-mode-base-map "\e\t" 'indent-for-tab-command)
;;;(define-key c-mode-base-map "\177" 'kill-region)
;;;(define-key c-mode-base-map "\^c\^c" 'c-comment-region)
	 (define-key c-mode-base-map "\^c\c" 'c-ifdef-region)
	 (define-key c-mode-base-map "\^xf" 'c-fill-paragraph)
	 (define-key c-mode-base-map "\^xc" 'c-fill-paragraph)
	 (define-key c-mode-base-map "\^]" 'visit-tag-at-point)
	 (define-key c-mode-base-map "\^^" 'return-from-tag)
	 (define-key c-mode-base-map "\eq" 'query-replace-regexp)
	 (modify-syntax-entry 95 "w"))))


;;; cperl-mode is preferred to perl-mode
(add-hook 'cperl-mode-hook
	  (function
	   (lambda ()
	     (cperl-set-style "PerlStyle")
	     (defconst indent-tabs-mode nil)
	     (setq cperl-auto-newline nil)
	     (setq cperl-electric-parens nil)
	     (setq cperl-electric-keywords t)
	     (set-face-foreground 'cperl-array-face "SlateBlue")
	     (set-face-foreground 'cperl-hash-face "Khaki")
             (define-key cperl-mode-map "\r" 'cperl-linefeed)
             (define-key cperl-mode-map "\n" 'newline-and-indent))))


(add-hook 'emacs-lisp-mode-hook
          (function
           (lambda ()
	     (defconst indent-tabs-mode nil)
             (define-key emacs-lisp-mode-map "\t" 'indent-according-to-mode)
             (define-key emacs-lisp-mode-map "\r" 'newline-and-indent)
             (define-key emacs-lisp-mode-map "\n" 'newline))))

(add-hook 'python-mode-hook
          (function
           (lambda ()
	     (defconst indent-tabs-mode nil)
             (local-set-key (kbd "RET") 'newline-and-indent))))


(setq auto-mode-alist
      (append '(
;;		("\\.cgi$"      . cperl-mode)
		("\\.t$"        . cperl-mode)
		("\\.js$"       . java-mode)
                ("\\.m$"        . octave-mode)
;; 		("\\.htm[l]*$"  . html-helper-mode)
;; 		("\\.shtm[l]*$" . html-helper-mode)
;;              ("\\.esi$"      . xml-mode)
;;              ("\\.esi_data$" . xml-mode)
		)  auto-mode-alist ))


;;; set up for perforce source control mode
;;; (load-library "p4")

;;; set up html-helper-mode
;; (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;; (setq html-helper-address-string
;;       "<a href=\"mailto:jwm@emptech.com\">Jeff McCarrell &lt;jwm@emptech.com&gt;</a>")
;; (add-hook 'html-helper-load-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (setq html-helper-use-expert-menu t)
;; 	     (html-helper-rebuild-menu);
; 	     ;;; show tracing info about html indenting
;; 	     (setq html-helper-print-indent-info t)
;; 	     (turn-on-font-lock)
;; 	     )))

;; ;;; set up erlang mode on my osx box proteus
;; (cond ((file-exists-p "/usr/local/lib/erlang/lib")
;;        (setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.5.1/emacs"
;;                              load-path))
;;        (setq erlang-root-dir "/usr/local/lib/erlang")
;;        (setq exec-path (cons "/usr/local/bin" exec-path))
;;        (require 'erlang-start)))

;; ;;; load up support for R in emacs when installed.
;; (when (locate-library "ess-site") (require 'ess-site))

;; ;;; set up nxhtml for php-mode when installed
(when (locate-library "nxhtml/autostart.el")
  (load (locate-library "nxhtml/autostart.el")))

;; load in support for ccrypt encryption and decryption when installed
(when (locate-library "ps-ccrypt.el")
  (load (locate-library "ps-ccrypt.el")))

;; load in support for bcrypt when installed
(when (locate-library "crypt.el")
  (load (locate-library "crypt.el")))

;; enable en/decrypting of .gpg files
;; (when (locate-library "epa-file.el")
;;   (require 'epa-file)
;;   (epa-file-enable))
