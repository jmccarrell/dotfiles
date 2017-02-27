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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
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

;; prefer utf-8 encoding in all cases.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
(setq require-final-newline t)

;; prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Turn off the blinking cursor
(blink-cursor-mode -1)

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
(setq shell-command-switch "-ic")

;; Don't beep at me
(setq visible-bell t)

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


;;; Configure libraries

(use-package solarized-theme
  :init
  (progn
    (load-theme 'solarized-dark t))
  :ensure t)

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
