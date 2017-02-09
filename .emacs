;;; jwms emacs startup file
;;;

;; set up emacs loading paths; add-to-list pushes onto the front of 'load-path, so
;;   higher precedence is given to later calls here

;; begin-jwm package.el support
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package) ;; You might already have this line
;; begin-jwm MELPA support
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; end-jwm MELPA

;; package-refresh-contents causes a pause for every emacs invocation
;;  as it makes the network connection to the package archives.
;;  especially annoying when git invokes emacs as my EDITOR
;;  so make this conditional on emacs being started under a window system
;;  and attempt to make EDITOR="emacs --no-window-system"
(package-initialize)
(when window-system
  (package-refresh-contents))
;; end-jwm package.el


;; add in the brew emacs directory when it exists
(cond ((file-exists-p "/usr/local/share/emacs/site-lisp")
       (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")))

;; add in my dotfile-controlled emacs lisp when it is available
(cond ((file-exists-p "~/.emacs.jwm.d/elisp")
       (add-to-list 'load-path "~/.emacs.jwm.d/elisp")))

;; set up my customization file
;; this is cut-and-paste from the emacs info node "Saving Customizations"
(cond ((< emacs-major-version 24)
       (setq custom-file "~/.emacs.jwm.d/emacs-custom-23.el"))
      ((= emacs-major-version 24)
       (setq custom-file "~/.emacs.jwm.d/emacs-custom-24.el"))
      (t
       ;; past Emacs 24
       (setq custom-file "~/.emacs.jwm.d/emacs-custom-future.el")))

(load custom-file)

(setq user-full-name "Jeff McCarrell")
(setq user-mail-address "jeff@mccarrell.org")

;; prefer utf-8 encoding in all cases.
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'markdown-mode-hook #'yas-minor-mode)

;; always show syntax highlighting
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; reveal trailing whitespace
;; I am using the default; there is a fancier mechanism here:
;;   http://www.emacswiki.org/emacs/whitespace.el
;; (when (>= emacs-major-version 22)
;;   (setq-default show-trailing-whitespace t))

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

;;; set up some key bindings
(define-key global-map "\C-xy" 'revert-buffer)
(define-key global-map "\C-c\C-c" 'comment-region)
(define-key global-map "\C-x\C-e" 'compile)
(define-key global-map "\e\C-g" 'goto-line)
;; (define-key global-map "\C-x\C-z" 'shell)
;; (define-key global-map "\C-z" 'shell)

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

;; I prefer solarized-dark
(when window-system
  (when (locate-library "solarized")
    (message "found and loading solarized theme")
    (let ((theme-loaded (load-theme 'solarized-dark t)))
      (if theme-loaded (message "solarized loaded success") (message "solarized load failed")))))

;; begin-jwm: xterm-color config as of 20170102.1525
;; xterm-color config from: https://github.com/atomontage/xterm-color
;; comint install
(require 'xterm-color)
(progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
       (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

;; comint uninstall
;; (progn (remove-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; For M-x shell, also set TERM accordingly (xterm-256color)

;; You can also use it with eshell (and thus get color output from system ls):

(require 'eshell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;;  Don't forget to setenv TERM xterm-256color
;; end-jwm: xterm-color config as of 20161104.1949


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


(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "k&r")))

(add-hook 'c-mode-hook
      (function
       (lambda ()
         (c-set-style "k&r")
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


;;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-shell-interpreter "python3")))


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

;;; set up nxhtml for php-mode when installed
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

;; load in support for editing yaml files when availble.
;;  I got my yaml-mode from git@github.com:yoshiki/yaml-mode.git
(when (locate-library "yaml-mode.el")
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;;; set up my visual preferences

;;; preferences when under a window system, which these days is pretty much os x
(when window-system
  (setq default-frame-alist
        (list '(menu-bar-lines . 1)
              '(tool-bar-lines . 0)
              '(vertical-scroll-bars . nil)
              '(width . 100)
              '(height . 52)
              ;; '(foreground-color . "white")
              ;; '(foreground-color . "#839496")  ;; foreground from solarized-emacs
              ;; I think the solarized foreground is a little too quiet;
              ;;  it corresponds about to grey-44 in the standard colors in list-colors-display
              ;; so bring it up a notch or two
              '(foreground-color . "grey54")  ;; foreground from solarized-emacs
              ;; '(background-color . "black")
              '(background-color . "#002b36")  ;; background from solarized-emacs
              ;; '(cursor-color . "DarkOrange")
              '(cursor-color . "DarkOrange3")  ;; tone down cursor some with solarized colors
              ))
  ;;; (add-to-list 'default-frame-alist '(font . "lucidasanstypewriter-14"))
  (cond ((and (>= emacs-major-version 23) (eq 'darwin system-type))
         (add-to-list 'default-frame-alist '(font . "Monaco-14")))
        ((eq 'gnu/linux system-type)
         (message "jwm: display height %d" (display-pixel-height))
         (cond ((>= (display-pixel-height) 1800)
                (message "jwm: detected high res monitor.")
                (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-18")))
               (t
                (message "jwm: default monitor size chosen.")
                (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-11")))
               ))
        (t
         ;; default to changing nothing
         t)
        )
  )

;;; preferences on a raw terminal
(unless window-system             ;;; terminal, not window system
  (set-face-foreground 'minibuffer-prompt "wheat3")
  (set-face-foreground 'dired-ignored "rosybrown3")
  (set-face-foreground 'file-name-shadow "wheat1")
  (set-face-foreground 'file-name-shadow "wheat1")
  (set-face-foreground 'shadow "wheat1")
)

;;; on OS X, set binding for meta to be command key, next to space bar
;;;  disable meaning of option key, so it is passed into emacs.
;;;  I use these semantics so that, e.g., option-v gives me the square root character.
;;;
;;; I have had this code much higher in this init file; however, I run into eval order
;;;  issues.  When I leave it here late in the sequence, it seems to do what I want.
(when (and window-system (eq 'ns window-system))
  (set-variable (quote mac-option-modifier) 'none))


;;;
;;; load intero; racier haskell-mode
;;;
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
