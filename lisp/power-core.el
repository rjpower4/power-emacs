;;; power-core.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Recent files
(use-package recentf
  :defer 1
  :ensure nil
  :commands recentf-open-files
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)
  ;; Add to recent files if save to file
  (add-hook 'write-file-functions
            (lambda ()
              (when buffer-file-name
                (recentf-add-file buffer-file-name))
              nil)))

;; Defer this for a little
(use-package uniquify
  :defer 2
  :ensure nil)

;; Go back to where I was
(use-package saveplace
  :defer 2
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
  (add-to-list 'recentf-exclude (expand-file-name "elpa/" user-emacs-directory))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; Automatically update packages
(use-package auto-package-update
  :defer 3
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

;;; Text scaling
(defun power/text-scale-default ()
  "Set the text scale to the default value."
  (interactive)
  (text-scale-set 0))

;; Pull in the PATH variable from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env
   "JULIA_DEPOT_PATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Formatting
(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(provide 'power-core)
;;; power-core.el ends here
