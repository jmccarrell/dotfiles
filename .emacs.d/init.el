(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  ;; (mapc #'(lambda (path)
  ;;           (add-to-list 'load-path
  ;;                        (expand-file-name path user-emacs-directory)))
  ;;       '("jwm-elisp" "site-lisp" "override" "lisp" "lisp/use-package" ""))

  (require 'cl)

  (defvar use-package-verbose t)
  ;;(defvar use-package-expand-minimally t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

;;;
;;; jwm: leave the custom settings in settings.el
;;;  it is good enough for jwiegley
(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)

;;; Enable disabled commands

(put 'downcase-region             'disabled nil)   ; Let downcasing work
(put 'erase-buffer                'disabled nil)
(put 'eval-expression             'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page              'disabled nil)   ; Let narrowing work
(put 'narrow-to-region            'disabled nil)   ; Let narrowing work
(put 'set-goal-column             'disabled nil)
(put 'upcase-region               'disabled nil)   ; Let upcasing work
;; (put 'company-coq-fold            'disabled nil)
;; (put 'TeX-narrow-to-group         'disabled nil)
;; (put 'LaTeX-narrow-to-environment 'disabled nil)

;;; jeffs settings

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; get rid of all of the backup files
(setq backup-before-writing nil)
(setq make-backup-files nil)
;; Keep all backup and auto-save files in one directory
;; (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;; (setq auto-save-list-file-prefix
;;       (expand-file-name "~/tmp/emacs/emacs-saves"))

;; prefer utf-8 encoding in all cases.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq inhibit-startup-message t)
;; needed for emacs23
(setq inhibit-splash-screen t)

;; complete things without hesitation
(setq completion-auto-help nil)
(setq completion-ignore-case t)
;; (define-key minibuffer-local-completion-map " " 'minibuffer-complete)
;; (define-key minibuffer-local-must-match-map " " 'minibuffer-complete)

;; key some global key bindings I happen to like.
(define-key global-map "\C-xy" 'revert-buffer)
(define-key global-map "\C-x\C-e" 'compile)
(define-key global-map "\e\C-g" 'goto-line)

;;; no longer needed with kill-visual-line
;; kill the whole line when at the beginning of it
;;(setq kill-whole-line t)

;; use the gnome/X cut buffers for killing and yanking
(setq select-enable-clipboard t)
;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
(setq require-final-newline t)

;; prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; set a high fill column
(setq-default fill-column 108)

;; blink the cursor 10 times, then stop
(blink-cursor-mode 10)

(setq-default indicate-empty-lines t)

;; Don't count two spaces after a period as the end of a sentence.
;; Just one space is needed.
(setq sentence-end-double-space nil)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

;; jeff: turn off modes: paren, col-number
;; (show-paren-mode t)

(column-number-mode t)

(global-visual-line-mode)
(diminish 'visual-line-mode)

(setq uniquify-buffer-name-style 'forward)

;; -i gets alias definitions from .bash_profile
;; jwm: but when I turn this on, I get:
;; bash: cannot set terminal process group (-1): Inappropriate ioctl for device
;; bash: no job control in this shell
;; so turn it off
;; (setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

;; prefer xterm-color
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

;;; jeffs colors
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
;;; end jeffs colors

;;; frame settings under window system
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
                (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-16")))
               (t
                (message "jwm: default monitor size chosen.")
                (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-11")))
               ))
        (t
         ;; default to changing nothing
         t)
        )
  )

;;; on OS X, set binding for meta to be command key, next to space bar
;;;  disable meaning of option key, so it is passed into emacs.
;;;  I use these semantics so that, e.g., option-v gives me the square root character.
;;;
;;; I have had this code much higher in this init file; however, I run into eval order
;;;  issues.  When I leave it here late in the sequence, it seems to do what I want.

;;; circa Feb 2017, I want to move away from entering the special characters from the keyboard
;;;  and back toward using the keys at the bottom of the keyboard for meta keys.
;;; so this has to go.
;; (when (and window-system (eq 'ns window-system))
;;   (set-variable (quote mac-option-modifier) 'none))

;;; now invert the bindings of meta and super on the mac
;;;  so that meta is closest to the space bar (command key on the mac keyboard)
;;;          super is to the left of meta     (option/alt key on the mac keyboard)
;;;  and leave right option unbound to insert accented characters.
;;;
;;; possible mac variables to bind
;;  	mac-command-modifier
;; mac-control-modifier 	mac-function-modifier
;; mac-option-modifier 	mac-right-command-modifier
;; mac-right-control-modifier 	mac-right-option-modifier
(when (and window-system (eq 'ns window-system))
  (message "setting mac meta/super key bindings")
  (set-variable (quote mac-option-modifier) 'super)
  (set-variable (quote mac-command-modifier) 'meta)
  (set-variable (quote mac-right-option-modifier) 'none))


;;; Configure libraries

;; ag config dervied from danielmai's config
(use-package ag
  :commands ag
  :ensure t)

;; a better ace-jump-mode; derivedd from jwiegley
(use-package avy
  :ensure t
  :load-path "site-lisp/avy"
  :bind ("M-h" . avy-goto-char)
  :config
  (avy-setup-default))

;; helm config derived from danielmai's config
(use-package helm
  :ensure t
  :diminish helm-mode
  :init (progn
          (require 'helm-config)
          ;; (use-package helm-projectile
          ;;   :ensure t
          ;;   :commands helm-projectile
          ;;   :bind ("C-c p h" . helm-projectile))
          (use-package helm-ag :defer 10  :ensure t)
          (setq helm-locate-command "mdfind -interpret -name %s %s"
                helm-ff-newfile-prompt-p nil
                helm-M-x-fuzzy-match t)
          (helm-mode)
          ;; (use-package helm-swoop
          ;;   :ensure t
          ;;   :bind ("H-w" . helm-swoop))
          )
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-`" . helm-resume)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package macrostep
  :ensure t
  :bind ("C-c e m" . macrostep-expand))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package nxml-mode
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (defun my-nxml-mode-hook ()
    (bind-key "<return>" #'newline-and-indent nxml-mode-map))

  (add-hook 'nxml-mode-hook 'my-nxml-mode-hook)

  (defun tidy-xml-buffer ()
    (interactive)
    (save-excursion
      (call-process-region (point-min) (point-max) "tidy" t t nil
                           "-xml" "-i" "-wrap" "0" "-omit" "-q" "-utf8")))

  (bind-key "C-c M-h" #'tidy-xml-buffer nxml-mode-map)

  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (add-hook 'nxml-mode-hook 'hs-minor-mode)

  ;; optional key bindings, easier than hs defaults
  (bind-key "C-c h" #'hs-toggle-hiding nxml-mode-map))

(use-package solarized-theme
  :init
  (progn
    (load-theme 'solarized-dark t))
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (concat user-emacs-directory "snippets"))
  (yas-global-mode))

;;; Post initialization

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;; init.el ends here
