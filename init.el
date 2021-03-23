;;; init.el -*- lexical-binding: t; -*-
;;
;; Author: Rolfe Power
;;
;; __________                         ___________                             
;; \______   \______  _  __ __________\_   _____/ _____ _____    ____   ______
;;  |     ___/  _ \ \/ \/ // __ \_  __ \    __)_ /     \\__  \ _/ ___\ /  ___/
;;  |    |  (  <_> )     /\  ___/|  | \/        \  Y Y  \/ __ \\  \___ \___ \ 
;;  |____|   \____/ \/\_/  \___  >__| /_______  /__|_|  (____  /\___  >____  >
;;                             \/             \/      \/     \/     \/     \/
;;
;; Send it my guy...

;;
;;; Load Early init if it wasn't loaded
(unless (boundp 'power-version)
  (load (concat (file-name-directory load-file-name) "early-init")
	    nil t))

;;
;;; Garbage Collection
;; Use this threshold
(defvar power-gc-cons-threshold (* 128 (* 1024 1024)))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold power-gc-cons-threshold)))

;;
;;; About me
(setq user-full-name       "Rolfe Power"
      user-mail-address    (rot13 "ebysrcbjre4@tznvy.pbz"))


;;
;;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;
;;; Constants & Variables
(defconst power-version "0.0.1" "Current version of PowerEmacs")
(defconst power-init-p nil "Non-nil if PowerEmacs has been initialized")
(defconst power-interactive-p (not noninteractive) "If non-nil, Emacs is interactive")
(defconst power-debug-p (or (getenv-internal "DEBUG") init-file-debug) "Log more?")
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkely-unix)))
(defconst power-emacs-dir user-emacs-directory)
(defconst power-leader-key "M-SPC" "Leader prefix key")
(defvar power-font nil "Default font")
(defvar power-variable-pitch-font nil "The default font for variable-pitch text.")
(defvar doom-serif-font nil "The default font for `fixed-pitch-serif' face.")

;;
;;; UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless IS-WINDOWS (setq selection-coding-system 'utf-8))

;;
;;; Quiet startup
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message "Welcome to PowerEmacs...\n\n")

;;
;;; Optimizations
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 (* 1024 1024)) ; 16 mb
      gcmh-verbose power-debug-p)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(unless IS-LINUX (setq command-line-x-option-alist nil))
(unless IS-MAC   (setq command-line-ns-option-alist nil))

;;
;;; Package setup

;; Repositories
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(package-initialize)

;; Use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;;
;;; Basic Packages

;; Recent files
(use-package recentf
  :ensure nil
  :after no-littering
  :commands recentf-open-files
  :hook (after-init . recentf-mode)
  :config
  (require 'no-littering)
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)
  ;; Add to recent files if save to file
  (add-hook 'write-file-functions
            (lambda ()
              (when buffer-file-name
                (recentf-add-file buffer-file-name))
              nil)))

;; Go back to where I was
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-limit 100))

;; Hide the minor modes
(use-package diminish
  :demand t)

;; Clean up the emacs-user-directory
(use-package no-littering
  :config
  (require 'recentf)
  (require 'saveplace)
  (setq save-place-file (no-littering-expand-var-file-name "saveplace"))
  (setq recentf-save-file (no-littering-expand-var-file-name "recentf"))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Automatically update packages
(use-package auto-package-update
  :after no-littering
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (require 'no-littering)
  (setq auto-package-update-last-update-day-path
        (expand-file-name "last-package-update-day" no-littering-etc-directory))
  (setq auto-package-update-last-update-day-filename "last-package-update-day")
  (auto-package-update-maybe))

;;
;;; Keybinding packages
(require 'power-keys)

;;
;;; Extensions
;; Collection of Ridiculously Useful eXtensions
(use-package crux
  :after general
  :general
  ("C-a" 'crux-move-beginning-of-line)
  ("<C-M-return>" 'crux-smart-open-line-above)
  ("<M-return>" 'crux-smart-open-line)
  ("C-k" 'crux-smart-kill-line))

;;
;;; General UX settings
(setq confirm-kill-emacs nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      ring-bell-function #'ignore
      visible-bell nil)

;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Cursor
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; Fringes
(setq indicate-buffer-boundaries nil
      indicate-emtpy-lines nil)

;; Windows/Frames
(setq frame-title-format '("%b -- PowerEmacs")
      icon-title-format frame-title-format)

;; Don't resize in steps
(setq frame-resize-pixelwise t)
(setq window-resize-pizelwise nil)

;; Tool, menu, scrollbar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Avoid GUI
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizontal ones
(setq split-width-threshold 160
      split-height-threshold nil)

;; Minibuffer
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(setq resize-mini-windows 'grow-only)
(fset #'yes-or-no-p #'y-or-n-p)
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Line highlighting
(use-package hl-line
 :ensure nil
 :config
 (setq hl-line-sticky-flag nil
       global-hl-line-sticky-flag nil)
 (global-hl-line-mode 1))

;; Undo/Redo changes in window layout
(use-package winner
  :ensure nil
  :preface (defvar winner-dont-bind-my-keys t)
  :custom
  (winner-boring-buffers
   '("*Completions*"
     "*Compile-Log*"
     "*inferior-lisp*"
     "*Fuzzy Completions*"
     "*Apropos*"
     "*Help*"
     "*cvs*"
     "*Buffer List*"
     "*Ibuffer*"
     "*esh command on file*"))
  :config
  (winner-mode 1))

;; Line & Column Numbers
(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(column-number-mode 1)

;; Show time
(display-time-mode 1)

;; Highlight matching parens
(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
	show-paren-highlight-openparen t
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t)
  (show-paren-mode 1))

;; Form feed characters (^L) to horizontal lines
(use-package page-break-lines
  :diminish
  :init (global-page-break-lines-mode))

;; Resolve symlinks when opening files
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Ignore "X and Y are the same file."
(setq find-file-supress-same-file-warnings t)

;; Backups & Lockfiles
(setq create-lockfiles nil
      make-bakcup-files nil)

;; Autosave
(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (no-littering-expand-var-file-name "auto-save/")
      tramp-auto-save-directory (no-littering-expand-var-file-name "tramp-autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Spaces > Tabs
(setq-default indent-tabs-mode nil
	      tab-width 4)
(setq-default tab-always-indent nil)

;; Not too long now
(setq-default fill-column 80)

;; Continue wrapped words @ whitespace
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Who even does this?
(setq sentence-end-double-space nil)

;; POSIX
(setq require-final-newline t)

;; Clipboard
(setq kill-do-not-save-duplicates t)
(setq x-slect-request-type '(UTF8_STRING COMPOUND_TECXT TEXT STRING))

;;
;;; UX Packages
;; Smartparens
(use-package smartparens
  :diminish
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  ;; Probably writing lisp in minibuffer
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil))

;; So long
(use-package so-long
  :hook (after-init . global-so-long-mode)
  :config
  (setq so-long-threshold 400))

;;
;;; Theming
(require 'power-themes)

;;
;;; Text scaling
(defun power/text-scale-default ()
  "Set the text scale to the default value."
  (interactive)
  (text-scale-set 0))

(general-define-key
 "C-=" #'power/text-scale-default
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

;;
;;; Movement
;; Move forward and backward by paragraph
(general-define-key
 "M-n" #'forward-paragraph
 "M-p" #'backward-paragraph)

;; Ace Window
(use-package ace-window
  :general
  ("C-x C-o" 'ace-window))

;; Avy
(use-package avy
  :general
  ("C-; c" 'avy-goto-char-2)
  ("C-; l" 'avy-goto-line)
  ("C-; w" 'avy-goto-word-1)
  :custom
  (avy-timeout-seconds 0.3))

;;
;;; Org Mode
(use-package org
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-directory (concat (getenv "HOME") "/Documents/org"))
  (org-agenda-files (list org-directory))
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "%\*"))))
  (org-capture-templates '())
  (org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "ATTEND(a)" "QUESTION(q)"
                    "|"
                    "DONE(d)" "CANC(c)" "DEFR(f)")))
  (org-todo-keyword-faces
   '(("TODO"     . "red")
     ("PROG"     . "gold")
     ("WAIT"     . "orchid")
     ("ATTEND"   . "magenta")
     ("QUESTION" . "salmon")
     ("DONE"     . "green")
     ("CANC"     . "dark gray")
     ("DEFR"     . "steel blue")))
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-elipsis "⋯")
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))
  (use-package org-make-toc
    :hook (org-mode . org-make-toc-mode)))

;;
;;; Projectile
(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :init
  (setq projectile-cache-file (no-littering-expand-var-file-name "projectile.cache")
        projectile-auto-discover nil
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-known-projects-file (no-littering-expand-var-file-name
                                        "projectile.projects"))
  :config
  (projectile-mode +1))

;;
;;; Search
(use-package ivy
  :diminish
  :init
  (use-package amx :defer t)
  (use-package counsel
    :diminish
    :config
    (counsel-mode 1))
  (use-package swiper :defer t)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-err-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-wrap t))

;;
;;; Useful functions
(defun power/where-am-i ()
  "Tell me where I am."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))

;;
;;; Terminals
(use-package vterm)

;;
;;; Key Bindings
(power-key-map
  "M-SPC" '(swiper :wk "swiper")
  
  "h"   '(:ignore t :which-key "help")
  "h f" '(helpful-callable :which-key "function")
  "h v" '(helpful-variable :wk "variable")
  "h k" '(helpful-key :wk "key")

  "w"   '(:ignore t :wk "window")
  "w w" '(ace-window :wk "ace-window")
  "w d" '(delete-window :wk "delete")

  "b"   '(:ignore t :wk "buffer")
  "b b" '(counsel-buffer-or-recentf :wk "switch")
  "b B" '(counsel-ibuffer :wk "ibuffer")
  "b d" '(kill-current-buffer :wk "delete")
  "b D" '(kill-buffer-and-window :wk "delete with window")

  "f"   '(:ignore t :wk "file")
  "f f" '(counsel-find-file :wk "find file")
  "f r" '(counsel-recentf :wk "recentf")
  "f s" '(save-buffer :wk "save buffer")
  "f i" '(crux-find-user-init-file :wk "open init")

  "s"   '(:ignore t :wk "search")
  "s s" '(swiper :wk "swiper")
  "s r" '(counsel-rg :wk "ripgrep")

  "j"   '(:ignore t :wk "jump")
  "j c" '(avy-goto-char-2 :wk "char")
  "j d" '(dired-jump :wk "dired")
  "j l" '(avy-goto-line :wk "line")
  "j w" '(avy-goto-word-1 :wk "word")
  
  "o"   '(:ignore t :which-key "open")
  "o t" '(vterm :which-key "vterm")
  "o e" '(eshell :which-key "eshell"))

(provide 'init)
