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

;; Benchmark startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

;; --------------------------------------------------------------------------- #
;;; Garbage Collection
;; --------------------------------------------------------------------------- #
;; Use this threshold
(defvar power-gc-cons-threshold (* 128 (* 1024 1024)))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold power-gc-cons-threshold)))

;; --------------------------------------------------------------------------- #
;;; Load path
;; --------------------------------------------------------------------------- #
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

;; --------------------------------------------------------------------------- #
;;; Constants
;; --------------------------------------------------------------------------- #
;; Version information
(defconst power-version "0.0.2" "Current version of power emacs.")

;; System Information
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkely-unix)))

;; Directories
(defconst power-emacs-dir user-emacs-directory)
(defconst power-core-dir (concat power-emacs-dir "core/"))
(defconst power-local-dir (concat power-emacs-dir ".local/"))
(defconst power-etc-dir (concat power-emacs-dir "etc/"))
(defconst power-cache-dir (concat power-emacs-dir "cache/"))

;; About Me
(setq calendar-latitude 40.424660)
(setq calendar-longitude -86.848460)

;; Theming preferences
(defconst power-auto-theme nil)

;; --------------------------------------------------------------------------- #
;;; Startup
;; --------------------------------------------------------------------------- #
;; Make it quiet, keep it down if you will
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode)

;; --------------------------------------------------------------------------- #
;;; UTF-8 Encoding
;; --------------------------------------------------------------------------- #
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(unless IS-WINDOWS (setq selection-coding-system 'utf-8))

;; --------------------------------------------------------------------------- #
;;; Optimizations
;; --------------------------------------------------------------------------- #
(setq auto-mode-case-fold nil)
(setq-default bidi-display-reordering  'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq ffap-machine-p-known 'reject)
(setq frame-inhibit-implied-resize t)
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 (* 1024 1024)) ; 16 mb
      gcmh-verbose nil)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(unless IS-LINUX (setq command-line-x-option-alist nil))
(unless IS-MAC   (setq command-line-ns-option-alist nil))

;; --------------------------------------------------------------------------- #
;;; Security
;; --------------------------------------------------------------------------- #
(setq gnutls-verify-error t)

;; --------------------------------------------------------------------------- #
;;; Emacs Directory Littering
;; --------------------------------------------------------------------------- #
(setq async-byte-compile-log-file  (concat power-etc-dir "async-bytecomp.log")
      custom-file                  (concat power-etc-dir "custom.el")
      desktop-dirname              (concat power-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat power-cache-dir "pcache/")
      request-storage-directory    (concat power-cache-dir "request")
      auto-save-list-file-prefix   (concat power-cache-dir "auto-save/")
      tramp-auto-save-directory    (concat power-cache-dir "tramp-autosave/")
      shared-game-score-directory  (concat power-etc-dir "shared-game-score/"))

;; --------------------------------------------------------------------------- #
;;; Straight and Use Package Setup
;; --------------------------------------------------------------------------- #
;; Set variables for straight
(setq straight-base-dir (file-truename power-local-dir)
      straight-check-for-modifications '(watch-files find-when-checking)
      straight-build-dir (format "build-%s" emacs-version))

;; Define function to get straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use package
(straight-use-package 'use-package)

;; --------------------------------------------------------------------------- #
;;; General UX settings
;; --------------------------------------------------------------------------- #
(setq confirm-kill-emacs nil

      ;; Buffers
      confirm-nonexistent-file-or-buffer nil
      uniquify-buffer-name-style 'post-forward-angle-brackets

      ;; Bell
      ring-bell-function #'ignore
      visible-bell nil

      ;; Scrolling
      hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2

      ;; Fringes
      indicate-buffer-boundaries nil
      indicate-emtpy-lines nil

      ;; Windows/Frames
      frame-title-format '("%b -- PowerEmacs")
      icon-title-format frame-title-format

      ;; Don't resize in steps
      frame-resize-pixelwise t
      window-resize-pizelwise nil

      ;; Resolve symlinks when opening files
      find-file-visit-truename t
      vc-follow-symlinks       t

      ;; Who even does this?
      sentence-end-double-space nil

      ;; POSIX
      require-final-newline t

      ;; Clipboard
      kill-do-not-save-duplicates t
      x-slect-request-type '(UTF8_STRING COMPOUND_TECXT TEXT STRING))

;; Cursor
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

;; Avoid GUI
(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

;; Favor vertical splits over horizonntal ones
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

;; Line & Column Numbers
(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(column-number-mode 1)

;; Show time
(display-time-mode 1)

;; Ignore "X and Y are the same file."
(setq find-file-supress-same-file-warnings t)

;; Backups & Lockfiles
(setq create-lockfiles nil
      make-backup-files nil)

(setq backup-directory-alist `(("." . ,(concat power-cache-dir "saves")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave
(setq auto-save-default t
      auto-save-include-big-deletions t
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
(setq-default fill-column 90)

;; Continue wrapped words @ whitespace
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Text scaling
(defun power/text-scale-default ()
  "Set the text scale to the default value."
  (interactive)
  (text-scale-set 0))

;; --------------------------------------------------------------------------- #
;;; Where Am I? 
;; --------------------------------------------------------------------------- #
(defun power/where-am-I ()
  "Give path to buffer if visiting file or buffer name, and add result to kill 
ring."
  (interactive)
  (message
   (kill-new (if (buffer-file-name)
                 (buffer-file-name)
               (buffer-name)))))

;; --------------------------------------------------------------------------- #
;;; Recent Files
;; --------------------------------------------------------------------------- #
;; TODO: Add function to push onto recentf on save
(use-package recentf
  :straight (recentf :type built-in)
  :commands recentf-open-files
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (concat power-cache-dir "recentf"))
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 0)
  (recentf-max-saved-items 300)
  :config
  (add-to-list 'recentf-exclude power-etc-dir)
  (add-to-list 'recentf-exclude power-cache-dir)
  (add-to-list 'recentf-exclude power-local-dir))

;; --------------------------------------------------------------------------- #
;;; Save Place
;; --------------------------------------------------------------------------- #
(use-package saveplace
  :straight (saveplace :type built-in)
  :hook (after-init . save-place-mode)
  :init 
  (setq save-place-limit 200)
  :custom
  (save-place-file (concat power-cache-dir "places")))

;; --------------------------------------------------------------------------- #
;;; Diminish
;; --------------------------------------------------------------------------- #
(use-package diminish
  :straight t)

;; --------------------------------------------------------------------------- #
;;; Line Highlighting
;; --------------------------------------------------------------------------- #
(use-package hl-line
  :straight (hl-line :type built-in)
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  :config
  (global-hl-line-mode 1))

;; --------------------------------------------------------------------------- #
;;; Winner Undo
;; --------------------------------------------------------------------------- #
;; Undo/Redo Changes in window layout
(use-package winner
  :straight (winner :type built-in)
  :preface
  (defvar winner-dont-bind-my-keys t)
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
  
;; --------------------------------------------------------------------------- #
;;; Parentheses
;; --------------------------------------------------------------------------- #
;; Highlight Matching Parens
(use-package paren
  :straight (paren :type built-in)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

;; --------------------------------------------------------------------------- #
;;; Form Feed Characters
;; --------------------------------------------------------------------------- #
(use-package page-break-lines
  :straight t
  :diminish
  :init
  (global-page-break-lines-mode))

;; --------------------------------------------------------------------------- #
;;; Pull in Environment Variables from Shell
;; --------------------------------------------------------------------------- #
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-copy-env "JULIA_DEPOT_PATH")
  (when (or IS-MAC IS-LINUX IS-BSD)
    (exec-path-from-shell-initialize)))

;; --------------------------------------------------------------------------- #
;;; Formatting
;; --------------------------------------------------------------------------- #
(use-package format-all
  :straight t
  :bind
  ("C-c C-f" . format-all-buffer))

;; --------------------------------------------------------------------------- #
;;; Which Key
;; --------------------------------------------------------------------------- #
(use-package which-key
  :straight t
  :diminish which-key-mode
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-idle-delay 0.3)
  (which-key-secondary-delay 0.3)
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-bottom))

;; --------------------------------------------------------------------------- #
;;; Keybinding Utility
;; --------------------------------------------------------------------------- #
(use-package general
  :straight t)

;; --------------------------------------------------------------------------- #
;;; Better Help
;; --------------------------------------------------------------------------- #
(use-package helpful
  :straight t
  :general
  ("C-h v" #'helpful-variable)
  ("C-h f" #'helpful-callable)
  ("C-h k" #'helpful-key))

;; --------------------------------------------------------------------------- #
;;; Collection of Ridiculously Useful eXtensions
;; --------------------------------------------------------------------------- #
(use-package crux
  :straight t
  :general
  ("C-a"          'crux-move-beginning-of-line)
  ("<C-M-return>" 'crux-smart-open-line-above)
  ("<M-return>"   'crux-smart-open-line)
  ("C-x K"        'crux-kill-other-buffers)
  ("C-k"          'crux-smart-kill-line)
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; --------------------------------------------------------------------------- #
;;; Dired
;; --------------------------------------------------------------------------- #
(use-package dired
  :straight (dired :type built-in)
  :config
  (global-auto-revert-mode t)                    ; Enable auto-reverting
  (put 'dired-find-alternate-file 'disabled nil) ; Reuse same dired buffer
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t)
  (dired-recursive-deletes 'top)        ; Ask about recursive deletes/copies
  (dired-recursive-copies  'top)        ; at top level only

  ;; Auto refresh dired but don't brag about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)

  (delete-by-moving-to-trash t)         ; Separation anxiety
  (load-prefer-newer t)                 ; Get newest

  (auto-revert-use-notify nil)
  (auto-revert-interval 3)              ; 3 seconds

  :hook
  ;; Use '-' to go up a directory
  (dired-mode . (lambda ()
                  (local-set-key (kbd "-") (lambda () (interactive)
                                             (find-alternate-file "..")))))

  :general
  (:keymaps 'dired-mode-map
            [mouse-2] 'dired-find-file
            "C-c C-q" 'wdired-change-to-wdired-mode))

;; --------------------------------------------------------------------------- #
;;; Disk Usage
;; --------------------------------------------------------------------------- #
(use-package disk-usage
  :straight t
  :commands (disk-usage))

;; --------------------------------------------------------------------------- #
;;; Org Mode
;; --------------------------------------------------------------------------- #
(use-package org
  :straight (org :type built-in)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-export-coding-system 'utf-8)
  (org-tags-column 0)
  (org-support-shift-se)
  (org-directory (concat (getenv "HOME") "/Dropbox/org"))
  (org-agenda-files (list org-directory
                          (expand-file-name "journal/" org-directory)))
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "%\*"))))
  (org-capture-templates '())
  (org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QUES(q)"
                    "|"
                    "DONE(d)" "DEFR(f)" "CANC(c)")))
  (org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROG" . "goldenrod")
          ("WAIT" . "orchid")
          ("QUES" . "salmon")
          ("DONE" . "forest green")
          ("DEFR" . "steel blue")
          ("CANC" . "dark gray")))
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (eval-after-load 'org-indent
    (lambda () (diminish 'org-indent-mode)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (octave . t)
     (python . t))))

(use-package org-journal
  :straight t
  :after org
  :config
  (customize-set-variable 'org-journal-dir (expand-file-name "journal/" org-directory))
  (customize-set-variable 'org-journal-file-format "%Y-%m-%d.org")
  (customize-set-variable 'org-journal-date-format "%e %b %Y (%A)")
  (customize-set-variable 'org-journal-date-prefix "#+TITLE: ")
  (customize-set-variable 'org-journal-time-prefix "* ")
  (customize-set-variable 'org-journal-time-format ""))

;; --------------------------------------------------------------------------- #
;;; Projectile
;; --------------------------------------------------------------------------- #
(use-package projectile
  :straight t
  :diminish
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :init
  (setq projectile-cache-file (concat power-cache-dir "projectile.cache")
        projectile-auto-discover nil
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-known-projects-file (concat power-cache-dir 
                                        "projectile.projects"))
  :config
  (use-package ripgrep :straight t)
  (projectile-mode +1))

;; --------------------------------------------------------------------------- #
;;; Selectrum
;; --------------------------------------------------------------------------- #
(use-package selectrum
  :straight t
  :custom
  (selectrum-fix-vertical-window-height t) ; Always show full height
  :config
  (add-hook 'after-init-hook 'selectrum-mode))

(use-package selectrum-prescient
  :straight t
  :custom
  (prescient-history-length 100)
  (prescient-save-file (concat power-cache-dir "prescient-stats"))
  :config
  (prescient-persist-mode +1)
  (selectrum-prescient-mode +1))

;; --------------------------------------------------------------------------- #
;;; Consult
;; --------------------------------------------------------------------------- #
(use-package consult
  :straight t
  :config
  (setq-default consult-project-root-function 'projectile-project-root)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window]
                  #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame]
                  #'consult-buffer-other-frame))

;; --------------------------------------------------------------------------- #
;;; Marginalia
;; --------------------------------------------------------------------------- #
(use-package marginalia
  :straight t
  :init
  (marginalia-mode)
    ;; Refresh Selectrum on marginalia cycle
  (advice-add #'marginalia-cycle :after
              (lambda ()
                (when (bound-and-true-p selectrum-mode)
   custom               (selectrum-exhibit 'keep-selected))))

  ;; Show Docs
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light
          nil)))

;; --------------------------------------------------------------------------- #
;;; Ace Window
;; --------------------------------------------------------------------------- #
(use-package ace-window
  :straight t
  :general
  ("C-x C-o" 'ace-window)
  :custom
  (aw-scope 'frame))

;; --------------------------------------------------------------------------- #
;;; Avy
;; --------------------------------------------------------------------------- #
(use-package avy
  :straight t
  :general
  ("C-; C-;" 'avy-goto-char-timer)
  ("C-; c"   'avy-goto-char-2)
  ("C-; w"   'avy-goto-word-1)
  ("C-; l"   'avy-goto-line)
  :custom
  (aw-keys '(?a ?s ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  (avy-timeout-seconds 0.3))

;; --------------------------------------------------------------------------- #
;;; Terminals & Shells
;; --------------------------------------------------------------------------- #
(use-package vterm
  :straight t
  :config
  (require 'ffap)
  :general
  (:keymaps 'vterm-copy-mode-map
            "<C-return>" 'ffap-other-window))

(use-package eshell
  :straight (eshell :type built-in)
  :custom
  (eshell-directory-name (concat power-local-dir "eshell/")))

;; --------------------------------------------------------------------------- #
;;; Company
;; --------------------------------------------------------------------------- #
(use-package company
  :straight t
  :diminish company-mode
  :hook ((LaTeX-mode prog-mode) . company-mode)
  :custom
  ;; Keep it to current mode
  (company-dabbrev-other-buffers t)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode eaf-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0.1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers t))
  ;;:config
  ;;(global-company-mode 1))

(use-package company-tabnine
  :defer t
  :config
  (setq company-idle-delay 0))

;; --------------------------------------------------------------------------- #
;;; So Long
;; --------------------------------------------------------------------------- #
(use-package so-long
  :straight (so-long :type built-in)
  :hook (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 400))

;; --------------------------------------------------------------------------- #
;;; Smart Parentheses
;; --------------------------------------------------------------------------- #
(use-package smartparens
  :straight t
  :diminish
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (require 'smartparens-latex)
  ;; Lisp
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-max-prefix-length 25)
  (sp-max-pair-length 4))

;; --------------------------------------------------------------------------- #
;;; Magit
;; --------------------------------------------------------------------------- #
(use-package magit
  :straight t
  :defer t
  :custom
  (transient-levels-file  (concat power-etc-dir "transient/levels")
  (transient-values-file  (concat power-etc-dir "transient/values")
  (transient-history-file (concat power-etc-dir "transient/history")
  :config
  (setq-default magit-diff-refine-hunk t)
  (use-package magit-todos :straight t)
  (require 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

;; --------------------------------------------------------------------------- #
;;; Theming
;; --------------------------------------------------------------------------- #
;; Icons
(use-package all-the-icons :straight t)

;; Doom Themes
(use-package doom-themes :straight t)

;; Modus Themes
(use-package modus-themes
  :straight t
  :custom
  (modus-themes-no-mixed-fonts t)
  (modus-themes-slanted-constructs t))

;; Theme Selection
(defvar power-light-theme 'modus-operandi)
(defvar power-dark-theme  'modus-vivendi)
(defvar power-default-theme power-dark-theme)

;; Day and Night Modesp
(defun power/night-mode ()
  "Set the theme to a dark mode"
  (interactive)
  (consult-theme power-dark-theme))

(defun power/day-mode ()
  "Set the theme to a light mode"
  (interactive)
  (consult-theme power-light-theme))

;; Function to Switch theme by sunlight
(defun power/switch-theme-by-sunlight ()
  "Select theme automatically based on time of day."
  (require 'solar)
  (let* ((cur-hour (string-to-number (substring (current-time-string) 11 13)))
         (sun-events (solar-sunrise-sunset (calendar-current-date)))
         (sunrise (caar sun-events))
         (sunset (caadr sun-events)))
    (if (and (> cur-hour sunrise) (< cur-hour sunset))
        (power/day-mode)
      (power/night-mode))))

;; Set Theme
(if power-auto-theme
    (power/switch-theme-by-sunlight)
  (consult-theme power-default-theme))

;; --------------------------------------------------------------------------- #
;;; Snippets
;; --------------------------------------------------------------------------- #
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (use-package yasnippet-snippets
    :straight t
    :after yasnippet)
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map ("C-c C-n" . yas-expand-from-trigger-key)))

;; --------------------------------------------------------------------------- #
;;; Flycheck 
;; --------------------------------------------------------------------------- #
(use-package flycheck
  :straight t
  :defer t
  :diminish
  :hook
  (after-init . global-flycheck-mode)
  :commands (flycheck-add-mode)
  :custom
  (flycheck-global-modes '(python-mode latex-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe
                              'right-margin)))

(use-package flycheck-color-mode-line
  :straight t
  :defer t
  :config 
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; --------------------------------------------------------------------------- #
;;; Flyspell
;; --------------------------------------------------------------------------- #
(use-package flyspell
  :straight (flyspell :type built-in)
  :diminish
  :if (executable-find "aspell")
  :hook
  (((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode))
  :custom
  (flyspell-issue-message-flat nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

;; --------------------------------------------------------------------------- #
;;; LSP
;; --------------------------------------------------------------------------- #
(use-package lsp-mode
  :straight t
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 1000)
  (read-process-output-max (* 1024 1024))
  ;(lsp-eldoc-hook nil)
  :hook
  ((python-mode LaTeX-mode) . lsp))

;; --------------------------------------------------------------------------- #
;;; Clojure
;; --------------------------------------------------------------------------- #
(use-package clojure-mode
  :straight t
  :config
  ;(add-hook 'clojure-mode #'smartparens-strict-mode)
  (use-package aggressive-indent
    :straight t
    :config
    (add-hook 'clojure-mode #'aggressive-indent-mode))
  (use-package clojure-mode-extra-font-locking
    :straight t))

(use-package cider
  :straight t
  :defer t
  :diminish
  :config
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  :custom
  (cider-repl-wrap-history t)
  (cider-repl-history-file (concat power-cache-dir "cider-history"))
  (cider-save-file-on-load t)
  (cider-font-lock-dynamically '(macro core function var)))

(use-package clj-refactor
  :straight t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

;; --------------------------------------------------------------------------- #
;;; Julia
;; --------------------------------------------------------------------------- #
(use-package julia-mode
  :straight t
  :defer t)

(use-package julia-repl
  :straight t
  :defer t
  :config
  (julia-repl-set-terminal-backend 'vterm)
  (set-language-environment "UTF-8")
  (julia-repl-set-terminal-backend 'vterm)
  :hook
  (julia-mode . julia-repl-mode))

;; --------------------------------------------------------------------------- #
;;; Python
;; --------------------------------------------------------------------------- #
(use-package pip-requirements
  :straight t
  :defer t)

(use-package anaconda-mode
  :straight t
  :defer t
  :config
  (add-hook 'python-mode-hook (lambda () (unless (file-remote-p default-directory)
                                           (anaconda-mode 1))))
  (add-hook 'anaconda-mode-hook (lambda () (anaconda-eldoc-mode
                                            (if anaconda-mode 1 0))))
  (use-package company-anaconda
    :straight t
    :config
    (add-to-list 'company-backends 'company-anaconda)))

(use-package lsp-pyright
  :straight t
  :defer t
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

;; --------------------------------------------------------------------------- #
;;; PDF Tools
;; --------------------------------------------------------------------------- #
(use-package pdf-tools
  :straight t
  :defer t
  :if (not IS-WINDOWS)
  :mode "\\.pdf\\'"
  :commands (pdf-loader-install)
  :custom
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install))

;; --------------------------------------------------------------------------- #
;;; LaTeX
;; --------------------------------------------------------------------------- #
(use-package auctex
  :straight t
  :defer t
  :bind (:map TeX-mode-map
              ("C-c C-o" . TeX-recenter-output-buffer)
              ("C-c C-l" . TeX-next-error)
              ("M-[" . outline-previous-heading)
              ("M-]" . outline-next-heading))
  :preface
  (defun power/switch-to-help-window (&optional ARG REPARSE)
    "Switches to the *TeX HelP* buffer after compxilation."
    (other-window 1))
  :custom
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-fontify-script nil)
  (font-latex-fontify-script nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  :config
  (advice-add 'TeX-next-error :after #'power-switch-to-help-window)
  (advice-add 'TeX-recenter-output-buffer :after #'power/switch-to-help-window)
  ;; the ":hook" doesn't work for this one... don't ask me why.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package reftex
  :defer t
  :straight (reftex :type built-in)
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  :config
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  (add-hook 'reftex-toc-mode-hook
            (lambda () (reftex-toc-rescan))))

(use-package bibtex
  :straight t
  :defer t
  :after auctex
  :hook (bibtex-mode . power-bibtex-fill-column)
  :preface
  (defun power-bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :straight t
  :defer t
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math
  :straight t
  :after (auctex company))

(use-package company-reftex
  :straight t
  :defer t
  :after (auctex company))

(use-package latex-preview-pane
  :straight t
  :defer t
  :config
  (latex-preview-pane-enable))

(use-package preview
  :straight (preview :type built-in)
  :defer t
  :ensure nil
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

;; --------------------------------------------------------------------------- #
;;; Rust
;; --------------------------------------------------------------------------- #
(use-package rustic
  :straight t
  :defer t)

;; --------------------------------------------------------------------------- #
;;; Keybindings
;; --------------------------------------------------------------------------- #
(general-define-key
 "M-n" #'forward-paragraph
 "M-p" #'backward-paragraph
 "C-=" #'power/text-scale-default
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

;; Windows
(general-create-definer window-definer
  :prefix "C-c w")

(window-definer
  "" '(:ignore t :wk "window")
  "v" '(split-window-right :wk "vertical split")
  "s" '(split-window-below :wk "horizontal split")
  "w" '(ace-window :wk "ace-window")
  "d" '(delete-window :wk "delete")
  "u" '(winner-undo :wk "winner undo")
  "o" '(other-window :wk "other"))

;; Help
(general-create-definer help-definer
  :prefix "C-c h")

(help-definer
  ""  '(:ignore t :wk "help")
  "f" '(helpful-callable :which-key "function")
  "v" '(helpful-variable :wk "variable")
  "k" '(helpful-key :wk "key")
  "a" '(consult-apropos :wk "apropos")
  "m" '(describe-mode :wk "mode")
  "M" '(consult-man :wk "man"))

;; Buffers
(general-create-definer buffer-definer
  :prefix "C-c b")

(buffer-definer
 ""  '(:ignore t :wk "buffer")
 "b" '(consult-buffer :wk "switch")
 "i" '(ibuffer :wk "ibuffer")
 "d" '(kill-current-buffer :wk "delete")
 "D" '(kill-buffer-and-window :wk "delete with window"))

;; Files
(general-create-definer file-definer
  :prefix "C-c f")

(file-definer
  ""  '(:ignore t :wk "file")
  "f" '(find-file :wk "find file")
  "r" '(consult-recent-file :wk "recentf")
  "s" '(save-buffer :wk "save buffer")
  "i" '(crux-find-user-init-file :wk "open init"))

;; Search
(general-create-definer search-definer
  :prefix "C-c s")

(search-definer
  ""  '(:ignore t :wk "search")
  "s" '(consult-line :wk "buffer")
  "r" '(consult-ripgrep :wk "ripgreg"))

;; Jumping
(require 'dired)
(general-create-definer jump-definer
  :prefix "C-;")

(jump-definer
 ""  '(:ignore t :wk "jump")
 "C-;" '(consult-line :wk "char timer")
 "c" '(avy-goto-char-2 :wk "char")
 "d" '(dired-jump :wk "dired")
 "l" '(avy-goto-line :wk "line")
 "L" '(consult-goto-line :wk "line by number")
 "w" '(avy-goto-word-1 :wk "word"))

;; Git
(general-create-definer git-definer
  :prefix "C-c g")

(git-definer
 ""  '(:ignore t :wk "git")
 "s" '(magit-status :wk "status"))


;; Project
(general-create-definer project-definer
  :prefix "C-c p")

(project-definer
 ""  '(:ignore t :wk "project")
 "t" '(projectile-run-vterm :wk "vterm")
 "g" '(projectile-vc :wk "git")
 "d" '(projectile-dired :wk "dired")
 "i" '(projectile-ibuffer :wk "ibuffer")
 "s" '(projectile-ripgrep :wk "search")
 "f" '(projectile-find-file :wk "find file")
 "p" '(projectile-switch-project :wk "switch project"))

;; Opening Things
(general-create-definer open-definer
  :prefix "C-c o")

(open-definer
  ""   '(:ignore t :which-key "open")
  "a"  '(org-agenda :wk "agenda")
  "t"  '(vterm :which-key "vterm")
  "e"  '(eshell :which-key "eshell")
  "E"  '(elfeed :wk "elfeed")
  "i"  '(circe :wk "irc"))

;;
;;(power-key-map
;;  "SPC" '(consult-line :wk "find in buffer"))

(provide 'init)
