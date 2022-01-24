;;; init.el --- power customization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rolfe Power
;;
;; Author: Rolfe Power <rolfepower4@gmail.com>
;; Created: 23 Jan 2022
;;
;;; Commentary:
;;
;; __________                         ___________                             
;; \______   \______  _  __ __________\_   _____/ _____ _____    ____   ______
;;  |     ___/  _ \ \/ \/ // __ \_  __ \    __)_ /     \\__  \ _/ ___\ /  ___/
;;  |    |  (  <_> )     /\  ___/|  | \/        \  Y Y  \/ __ \\  \___ \___ \ 
;;  |____|   \____/ \/\_/  \___  >__| /_______  /__|_|  (____  /\___  >____  >
;;                             \/             \/      \/     \/     \/     \/
;;
;;; Code:

;; ========================================================================================
;;; Garbage Collection and Benchmarking
;; ========================================================================================
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

(defvar power-gc-cons-threshold (* 128 (* 1024 1024)))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold power-gc-cons-threshold)))

;; ========================================================================================
;;; Basic Constants and Configuration Variables
;; ========================================================================================
(defconst power-version "0.0.3" "Current version of power emacs.")
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkely-unix)))

(defconst power-emacs-dir user-emacs-directory)
(defconst power-init-file (buffer-file-name))
(defconst power-local-dir (concat power-emacs-dir ".local/"))
(defconst power-cache-dir (concat power-local-dir "cache/"))
(defconst power-core-dir (concat power-local-dir "core/"))
(defconst power-etc-dir (concat power-local-dir "etc/"))
(defconst power-org-dir
  (if IS-WINDOWS
      "C:\\Users\\rolfe\\Dropbox\\Org"
    (concat (file-name-as-directory (getenv "HOME")) "Dropbox/Org/")))

(setq calendar-latitude 40.424660)
(setq calendar-longitude -86.848460)

;; ========================================================================================
;;; Startup
;; ========================================================================================
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

;; ========================================================================================
;;; Optimizations
;; ========================================================================================
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

;; ========================================================================================
;;; Security
;; ========================================================================================
(setq gnutls-verify-error t)

;; ========================================================================================
;;; Directory Clean-Up
;; ========================================================================================
(setq async-byte-compile-log-file  (concat power-etc-dir "async-bytecomp.log")
      custom-file                  (concat power-etc-dir "custom.el")
      desktop-dirname              (concat power-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      url-cache-directory          (concat power-cache-dir "url/")
      pcache-directory             (concat power-cache-dir "pcache/")
      request-storage-directory    (concat power-cache-dir "request")
      auto-save-list-file-prefix   (concat power-cache-dir "auto-save/")
      tramp-auto-save-directory    (concat power-cache-dir "tramp-autosave/")
      shared-game-score-directory  (concat power-etc-dir "shared-game-score/"))

;; ========================================================================================
;;; UX Customization
;; ========================================================================================
;; Windows font selection is weird
(when IS-WINDOWS
  (set-face-attribute 'default nil :font "JuliaMono-10")
  (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10"))

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

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

(setq split-width-threshold 160
      split-height-threshold nil)

(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(setq resize-mini-windows 'grow-only)
(fset #'yes-or-no-p #'y-or-n-p)
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq-default display-line-numbers-width 4)
(setq-default display-line-numbers-widen t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))
(column-number-mode 1)



(setq find-file-supress-same-file-warnings t)

(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . ,(concat power-cache-dir "saves")))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq auto-save-default t
      auto-save-include-big-deletions t
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-default tab-always-indent nil)

(setq-default fill-column 90)

(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(add-to-list 'mode-line-misc-info '("@" system-name) t)

(defun power/text-scale-default ()
  "Set the text scale to the default value."
  (interactive)
  (text-scale-set 0))

;; ========================================================================================
;;; Package setup
;; ========================================================================================
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
(setq use-package-always-ensure t)


;; ========================================================================================
;;; Key-Binding
;; ========================================================================================
(use-package general)

;; ========================================================================================
;;; Better Help
;; ========================================================================================
(use-package helpful
  :general
  ("C-h v" #'helpful-variable)
  ("C-h f" #'helpful-callable)
  ("C-h k" #'helpful-key))

(use-package which-key
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

(use-package marginalia
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

;; ========================================================================================
;;; Recent Files
;; ========================================================================================
(use-package recentf
  :ensure nil
  :commands recentf-open-files
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (concat power-cache-dir "recentf"))
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 0)
  (recentf-max-saved-items 300)
  :config
  (add-to-list 'recentf-exclude (concat power-emacs-dir "elpa"))
  (add-to-list 'recentf-exclude power-etc-dir)
  (add-to-list 'recentf-exclude power-cache-dir)
  (add-to-list 'recentf-exclude power-local-dir))

;; ========================================================================================
;;; Save MiniBuffer History
;; ========================================================================================
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300)
  :custom
  (savehist-file (concat power-cache-dir "history")))

;; ========================================================================================
;;; Time
;; ========================================================================================
(use-package time
  :ensure nil
  :config
  (display-time-mode 1)
  :custom
  (display-time-24hr-format t)
  (display-time-day-and-date t))

;; ========================================================================================
;;; Save Place
;; ========================================================================================
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-limit 500)
  :custom
  (save-place-file (concat power-cache-dir "places")))

;; ========================================================================================
;;; Undoing and Redoing Changes in Window Layout
;; ========================================================================================
(use-package winner
  :ensure nil
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

;; ========================================================================================
;;; Mode Hiding Utility
;; ========================================================================================
(use-package diminish)

;; ========================================================================================
;;; Formatting
;; ========================================================================================
(use-package format-all)

;; ========================================================================================
;;; Dealing with parentheses
;; ========================================================================================
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (show-paren-mode 1))

(use-package smartparens
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

;; ========================================================================================
;;; Dired
;; ========================================================================================
(use-package dired
  :ensure nil
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
                                             (find-alternate-file ".."))))))

(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; ========================================================================================
;;; Disk Usage
;; ========================================================================================
(use-package disk-usage
  :if IS-LINUX
  :commands (disk-usage))

;; ========================================================================================
;;; Org Mode
;; ========================================================================================
(use-package org
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-export-coding-system 'utf-8)
  (org-tags-column 0)
  (org-support-shift-se)
  (org-directory power-org-dir)
  (org-agenda-files (list org-directory))
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

;; ========================================================================================
;;; Window Management
;; ========================================================================================
(use-package ace-window
  :general
;;  ("C-x C-o" 'ace-window)
  :custom
  (aw-scope 'frame))

;; ========================================================================================
;;; Jumping Around
;; ========================================================================================
(use-package avy
  :general
  ("C-; C-;" 'avy-goto-char-timer)
  ("C-; w"   'avy-goto-word-1)
  ("C-; l"   'avy-goto-line)
  :custom
  (aw-keys '(?a ?s ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  (avy-timeout-seconds 0.3))

;; ========================================================================================
;;; Dealing with long files
;; ========================================================================================
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 400))

;; ========================================================================================
;;; Git
;; ========================================================================================
(use-package magit
  :defer t
  :custom
  (transient-levels-file  (concat power-etc-dir "transient/levels"))
  (transient-values-file  (concat power-etc-dir "transient/values"))
  (transient-history-file (concat power-etc-dir "transient/history"))
  :config
  (setq-default magit-diff-refine-hunk t)
  (use-package magit-todos)
  (require 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

;; ========================================================================================
;;; Selection
;; ========================================================================================
(use-package selectrum
  :custom
  (selectrum-fix-vertical-window-height t)
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :custom
  (prescient-history-length 100)
  (prescient-save-file (concat power-cache-dir "prescient-stats"))
  :config
  (prescient-persist-mode +1)
  (selectrum-prescient-mode +1))

;; ========================================================================================
;;; Consult
;; ========================================================================================
(use-package consult
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))

;; ========================================================================================
;;; Projectile
;; ========================================================================================
(use-package projectile
  :diminish
  :hook
  (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-cache-file (concat power-cache-dir "projectile.cache")
        projectile-auto-discover nil
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-known-projects-file (concat power-cache-dir 
                                               "projectile.projects"))
  :config
  (use-package ripgrep))

(use-package ibuffer-projectile
  :defer t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

;; ========================================================================================
;;; LSP
;; ========================================================================================
(use-package lsp-mode
  :defer t
  :commands (lsp ls-deferred)
  :custom
  (lsp-session-file (concat power-cache-dir "lsp-session-v1"))
  (lsp-server-install-dir (concat power-cache-dir "lsp-server-install/"))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; ========================================================================================
;;; Treemacs
;; ========================================================================================
(use-package treemacs
  :defer t
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file (concat power-cache-dir "treemacs-last-error-persist")
        treemacs-git-mode 'simple)
  :config
  (treemacs-follow-mode -1))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :after lsp)

;; ========================================================================================
;;; Crux
;; ========================================================================================
(use-package crux
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

;; ========================================================================================
;;; Spelling
;; ========================================================================================
(use-package flyspell
  :if (not IS-WINDOWS)
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook
  (((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode))
  :custom
  (flyspell-issue-message-flat nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

;; ========================================================================================
;;; Terms and shells
;; ========================================================================================
(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name (concat power-local-dir "eshell/"))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  (eshell-term-name "xterm-256color"))

(use-package vterm
  :if (not IS-WINDOWS))

;; ========================================================================================
;;; Pulling in shell variables
;; ========================================================================================
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "JULIA_DEPOT_PATH")
  (when (or IS-MAC IS-LINUX IS-BSD)
    (exec-path-from-shell-initialize)))

;; ========================================================================================
;;; Company
;; ========================================================================================
(use-package company
  :diminish company-mode
  :hook ((LaTeX-mode prog-mode) . company-mode)
  :custom
  (company-tooltip-limit 20)
  (company-show-numbers t)
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eaf-mode)))

(use-package company-tabnine
  :defer t)

;; ========================================================================================
;;; Flycheck
;; ========================================================================================
(use-package flycheck
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
  :defer t
  :config 
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


;; ========================================================================================
;;; C/C++
;; ========================================================================================
(use-package cc-mode
  :ensure nil
  :hook
  (c-mode-common . (lambda () (c-set-style "stroustrup")))
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init
    (modern-c++-font-lock-global-mode t)))

;; ========================================================================================
;;; Julia
;; ========================================================================================
(use-package julia-mode
  :defer t)

;; ========================================================================================
;;; Python
;; ========================================================================================
(use-package python
  :ensure nil
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package pyvenv)

;; ========================================================================================
;;; Rust
;; ========================================================================================
(use-package rustic)
  
;; ========================================================================================
;;; Data Files
;; ========================================================================================
(use-package csv-mode
  :defer t
  :general)
  ;;(:keymaps 'csv-mode-map
   ;;         "C-c m a" #'csv-align-fields
    ;;        "C-c m u" #'csv-unalign-fields
     ;;       "C-c m s" #'csv-sort-fields
       ;;     "C-c m S" #'csv-sort-numeric-fields
         ;;   "C-c m k" #'csv-kill-fields
           ;; "C-c m t" #'csv-transpose))

(use-package json-mode
  :defer t)

;; ========================================================================================
;;; PDF
;; ========================================================================================
 (use-package pdf-view
    :ensure pdf-tools
    :diminish (pdf-view-themed-minor-mode
               pdf-view-midnight-minor-mode
               pdf-view-printer-minor-mode)
    :defines pdf-annot-activate-created-annotations
    :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
    :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
           (pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
           (pdf-tools-enabled . pdf-isearch-minor-mode))
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
    :magic ("%PDF" . pdf-view-mode)
    :bind (:map pdf-view-mode-map
           ("C-s" . isearch-forward))
    :init
    (setq pdf-view-use-scaling t
          pdf-view-use-imagemagick nil
          pdf-annot-activate-created-annotations t)
    :config
    ;; Activate the package
    (pdf-tools-install t nil t nil))


;; ========================================================================================
;;; LaTeX
;; ========================================================================================
(use-package auctex
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
  :ensure nil
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
  :defer t
  :after auctex
  :hook (bibtex-mode . power-bibtex-fill-column)
  :preface
  (defun power-bibtex-fill-column ()
    "Ensures that each entry does not exceed 120 characters."
    (setq fill-column 120)))

(use-package company-auctex
  :defer t
  :after (auctex company)
  :config (company-auctex-init))

(use-package company-math
  :after (auctex company))

(use-package company-reftex
  :defer t
  :after (auctex company))

(use-package latex-preview-pane
  :defer t
  :config
  (latex-preview-pane-enable))

(use-package preview
  :ensure nil
  :defer t
  :ensure nil
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))

;; ========================================================================================
;;; Theming
;; ========================================================================================
(use-package modus-themes
  :after consult
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-completions 'nil)
  (modus-themes-no-mixed-fonts t)
  (modus-themes-diffs 'desaturated)
  (modus-themes-fringes 'intense)
  (modus-themes-lang-checkers 'colored-background)
  (modus-themes-links nil)
  (modus-themes-mail-citations nil)
  (modus-themes-mode-line nil)
  (modus-themes-org-blocks nil)
  (modus-themes-paren-match 'intense-bold)
  (modus-themes-prompts 'intense-gray)
  (modus-themes-region 'bg-only-no-extend)
  (modus-themes-scale-headings nil)
  (modus-themes-syntax 'yellow-comments-green-strings)
  (modus-themes variable-pitch-headings nil)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-slanted-constructs t)
  :config
  (consult-theme 'modus-vivendi))

(use-package all-the-icons)

(provide 'init)
;;; init.el ends here
