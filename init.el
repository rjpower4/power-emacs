;;; init.el --- power customization -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rolfe Power
;;
;; Author: Rolfe Power <rolfepower4@gmail.com>
;; Created: 23 Jan 2022
;;
;;; Commentary:
;;
;; This is a basic emacs configuration that I've put together by stealing stuff
;; from a lot of other configurations that I have seen.
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
            (message "Power Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

;; ========================================================================================
;;; Basic Constants and Configuration Variables
;; ========================================================================================
;; Emacs and Power Emacs information
(defconst power-version "0.0.4" "Current version of power emacs.")
(defconst EMACS27+   (> emacs-major-version 26))

;; -- System Information
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkely-unix)))

;; -- Directories
(defconst power-emacs-dir user-emacs-directory)
(defconst power-init-file (buffer-file-name))
(defconst power-local-dir (concat power-emacs-dir "local/"))
(defconst power-cache-dir (concat power-local-dir "cache/"))
(defconst power-core-dir (concat power-local-dir "core/"))
(defconst power-etc-dir (concat power-local-dir "etc/"))
(defconst power-org-dir
  (if IS-WINDOWS
      "C:\\Users\\rolfe\\Dropbox\\notes\\org\\"
    (concat (file-name-as-directory (getenv "HOME")) "Dropbox/Org/")))

;; -- Location
(setq calendar-latitude 40.424660)
(setq calendar-longitude -86.848460)

;; ========================================================================================
;;; Straight Bootstrap
;; ========================================================================================
(setq straight-base-dir (file-truename power-local-dir)
      straight-build-dir (format "build-%s" emacs-version))
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

;; ========================================================================================
;;; Configuration Infrastructure
;; ========================================================================================
;; Use the `use-package' style configuration
(straight-use-package 'use-package)

;; Keybinding
(use-package general
  :straight t)

;; Hiding Modes
(use-package diminish
  :straight t
  :config
  (diminish 'eldoc-mode))

;; ========================================================================================
;;; Basic Settings
;; ========================================================================================
(use-package power-defaults
  :straight nil
  :no-require t
  :init
  (setq-default bidi-display-reordering  'left-to-right
                bidi-paragraph-direction 'left-to-right
                cursor-in-non-selected-windows nil
                fill-column 90
                tab-always-indent nil
                truncate-lines t
                word-wrap t
                indent-tabs-mode nil
                display-line-numbers-width 4
                display-line-numbers-widen t)
  (add-to-list 'mode-line-misc-info '("@" system-name) t)
  
  (unless IS-LINUX (setq command-line-x-option-alist nil))
  (unless IS-MAC   (setq command-line-ns-option-alist nil))
  (when IS-WINDOWS
    (set-face-attribute 'default nil :font "JuliaMono-10")
    (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10"))
  (blink-cursor-mode -1)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1))
  (when IS-LINUX
    (setq x-gtk-use-system-tooltips nil))
  (fset #'yes-or-no-p #'y-or-n-p)
  (column-number-mode 1)
  :custom
  (inhibit-startup-message t)
  (inhibit-startup-echo-area-message user-login-name)
  (inhibit-default-init t)
  (initial-major-mode 'emacs-lisp-mode)
  
  (auto-mode-case-fold nil)
  (bidi-inhibit-bpa t)
  (highlight-nonselected-windows nil)
  (ffap-machine-p-known 'reject)
  (frame-inhibit-implied-resize t)
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 (* 1024 1024))) ; 16 mb
  (gcmh-verbose nil)
  (idle-update-delay 1.0)
  (redisplay-skip-fontification-on-input t)
  (gnutls-verify-error t)
  (async-byte-compile-log-file  (concat power-etc-dir "async-bytecomp.log"))
  (bookmark-file                (concat power-etc-dir "bookmarks"))
  (custom-file                  (concat power-etc-dir "custom.el"))
  (desktop-dirname              (concat power-etc-dir "desktop"))
  (desktop-base-file-name       "autosave")
  (desktop-base-lock-name       "autosave-lock")
  (tramp-persistency-file-name  (concat power-cache-dir "tramp"))
  (url-cache-directory          (concat power-cache-dir "url/"))
  (pcache-directory             (concat power-cache-dir "pcache/"))
  (request-storage-directory    (concat power-cache-dir "request"))
  (auto-save-list-file-prefix   (concat power-cache-dir "auto-save/"))
  (tramp-auto-save-directory    (concat power-cache-dir "tramp-autosave/"))
  (shared-game-score-directory  (concat power-etc-dir "shared-game-score/"))

  (confirm-kill-emacs nil)

  ;; Buffers
  (confirm-nonexistent-file-or-buffer nil)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)

  ;; Bell
  (ring-bell-function #'ignore)
  (visible-bell nil)

  ;; Scrolling
  (hscroll-margin 2)
  (hscroll-step 1)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))
  (mouse-wheel-scroll-amount-horizontal 2)

  ;; Fringes
  (indicate-buffer-boundaries nil)
  (indicate-emtpy-lines nil)

  ;; Windows/Frames
  (frame-title-format '("%b -- PowerEmacs"))
  (icon-title-format frame-title-format)

  ;; Don't resize in steps
  (frame-resize-pixelwise t)
  (window-resize-pizelwise nil)

  ;; Resolve symlinks when opening files
  (find-file-visit-truename t)
  (vc-follow-symlinks       t)

  ;; Who even does this?
  (sentence-end-double-space nil)

  ;; POSIX
  (require-final-newline t)

  ;; Clipboard
  (kill-do-not-save-duplicates t)
  (x-slect-request-type '(UTF8_STRING COMPOUND_TECXT TEXT STRING))

  (blink-matching-paren nil)
  (x-stretch-cursor nil)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (echo-keystrokes 0.02)
  (resize-mini-windows 'grow-only)
  (split-width-threshold 160)
  (split-height-threshold nil)
  (minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (find-file-supress-same-file-warnings t)
  (create-lockfiles nil)
  (backup-directory-alist `(("." . ,(concat power-cache-dir "saves"))))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (auto-save-default t)
  (auto-save-include-big-deletions t)
  (auto-save-file-name-transforms (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                              ;; Prefix tramp autosaves to prevent conflicts with local ones
                                              (concat auto-save-list-file-prefix "tramp-\\2") t)
                                        (list ".*" auto-save-list-file-prefix t)))
  (tab-width 4)
  (enable-recursive-minibuffers t)
  (truncate-partial-width-windows nil)

  (display-time-24hr-format t)
  (display-time-day-and-date t)

  :hook
  (minibuffer-setup . cursor-intangible-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  (after-init . (lambda () (display-time-mode 1)))
  

  :config)



;; ========================================================================================
;;; History
;; ========================================================================================
(use-package recentf
  :straight (:type built-in)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-save-file (concat power-cache-dir "recentf"))
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 0)
  (recentf-max-saved-items 300)
  :config
  (add-to-list 'recentf-exclude power-etc-dir)
  (add-to-list 'recentf-exclude power-cache-dir)
  (add-to-list 'recentf-exclude power-local-dir))

(use-package savehist
  :straight (:type built-in)
  :hook
  (after-init . savehist-mode)
  :custom
  (history-length 1000)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history))
  (savehist-autosave-interval 300)
  (savehist-file (concat power-cache-dir "history"))
  (put 'minibuffer-history         'history-length 50)
  (put 'file-name-history          'history-length 50)
  (put 'set-variable-value-history 'history-length 25)
  (put 'custom-variable-history    'history-length 25)
  (put 'query-replace-history      'history-length 25)
  (put 'read-expression-history    'history-length 25)
  (put 'read-char-history          'history-length 25)
  (put 'face-name-history          'history-length 25)
  (put 'bookmark-history           'history-length 25))

(use-package save-place
  :straight (:type built-in)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 1000)
  (save-place-file (concat power-cache-dir "places")))


;; ========================================================================================
;;; Window Management
;; ========================================================================================
(use-package winner
  :straight (:type built-in)
  :init
  (defvar winner-dont-bind-my-keys)
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
;;; Spelling
;; ========================================================================================
(if (and (not IS-WINDOWS) (executable-find "aspell"))
    (progn
      (dolist (hk '(text-mode-hook
                    outline-mode-hook
                    latex-mode-hook
                    org-mode-hook
                    markdown-mode-hook))
        (add-hook hk 'flyspell-mode))
      (setq flyspell-issue-message-flat nil
            ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))))

;; ========================================================================================
;;; Parentheses
;; ========================================================================================
(use-package power-parens
  :straight nil
  :no-require t
  :hook
  (after-init . show-paren-mode)
  (after-init . electric-pair-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; ========================================================================================
;;; Files and Dired
;; ========================================================================================
(use-package so-long
  :straight (:type built-in)
  :hook
  (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 400))

(use-package dired
  :straight (:type built-in)
  :config
  (global-auto-revert-mode t)                     ; Enable auto-reverting
  (put 'dired-find-alternate-file 'disabled nil) ; Reuse same dired buffer
  :custom
  (dired-listing-switches "-alh")
  (dired-dwim-target t)
  (dired-recursive-deletes 'top)        ; Ask about recursive deletes/copies
  (dired-recursive-copies  'top)       ; at top level only

  ;; Auto refresh dired but don't brag about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)

  (delete-by-moving-to-trash t)         ; Separation anxiety
  (load-prefer-newer t)                 ; Get newest

  (auto-revert-use-notify nil)
  (auto-revert-interval 3)              ; 3 seconds
  :hook
  ;; Use '-' to go up a directory
  (dired-mode . 
              (lambda () (local-set-key (kbd "-") (lambda () (interactive)
                                                    (find-alternate-file ".."))))))
;; ========================================================================================
;;; Crux and useful functions
;; ========================================================================================
(defun power/text-scale-default ()
  "Set the text scale to the default value."
  (interactive)
  (text-scale-set 0))

(use-package crux
  :straight t
  :general
  ("C-k" #'crux-smart-kill-line)
  ("C-<backspace>" #'crux-kill-line-backwards)
  ([remap move-beginning-of-line] #'crux-move-beginning-of-line)
  ([remap kill-whole-line] #'crux-kill-whole-line)
  ([(shift return)] #'crux-smart-open-line))

;; ========================================================================================
;;; Help
;; ========================================================================================
(use-package helpful
  :straight t
  :general
  ("C-h f"    #'helpful-callable
   "C-h v"    #'helpful-variable
   "C-h k"    #'helpful-key
   "C-c C-d"  #'helpful-at-point
   "C-h F"    #'helpful-function
   "C-h C"    #'helpful-command)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-idle-delay 0.3)
  (which-key-secondary-delay 0.3))

(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

;; ========================================================================================
;;; Theming
;; ========================================================================================
(use-package modus-themes
  :straight t
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-completions 'nil)
  (modus-themes-no-mixed-fonts t)
  (modus-themes-diffs 'desaturated)
  (modus-themes-fringes 'intense)
  (modus-themes-lang-checkers '(background))
  (modus-themes-links nil)
  (modus-themes-mail-citations nil)
  (modus-themes-mode-line nil)
  (modus-themes-org-blocks nil)
  (modus-themes-paren-match '(intense bold))
  (modus-themes-prompts '(intense gray))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-scale-headings nil)
  (modus-themes-syntax '(yellow-comments green-strings))
  (modus-themes-variable-pitch-headings nil)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-slanted-constructs t)
  :config
  (load-theme 'modus-vivendi t))

;; ========================================================================================
;;; Buffers
;; ========================================================================================
(use-package uniquify
  :straight (:type built-in)
  :custom
  (temp-buffer-max-height 8)
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; ========================================================================================
;;; Minibuffer
;; ========================================================================================
(use-package vertico
  :straight t
  :config
  (customize-set-variable 'vertico-cycle t)
  :custom
  (vertico-resize nil)
  (vertico-count 10)
  (vertico-count-format nil)
  :hook
  (after-init . vertico-mode))

(use-package orderless
  :straight t
  :config
  (customize-set-variable 'completion-styles '(orderless basic))
  (customize-set-variable 'completion-category-overrides
                          '((file (styles . (partial-completion)))))
  :custom
  (completion-category-defaults nil))

(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (setq-default marginalia--ellipsis "…"
                marginalia-align 'right
                marginalia-align-offset -1)
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :straight t
  :general
  ("C-." #'embark-act)
  :custom
  (prefix-help-command #'embark-prefix-help-command))

;; ========================================================================================
;;; Completion
;; ========================================================================================
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)                ; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; Enable auto completion
  (corfu-separator ?\s)          ; Orderless field separator
  (corfu-quit-at-boundary nil)   ; Never quit at completion boundary
  (corfu-quit-no-match nil)      ; Never quit, even if there is no match
  (corfu-preview-current nil)    ; Disable current candidate preview
  (corfu-preselect-first nil)    ; Disable candidate preselection
  (corfu-on-exact-match nil)     ; Configure handling of exact matches
  (corfu-echo-documentation nil) ; Disable documentation in the echo area
  (corfu-scroll-margin 5)
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode))

;; ========================================================================================
;;; Snippets
;; ========================================================================================
(use-package yasnippet
  :straight t
  :defer t
  :config
  (use-package yasnippet-snippets
    :straight t)
  (yas-global-mode)
  (diminish 'yas-minor-mode))

;; ========================================================================================
;;; Consult
;; ========================================================================================
(use-package consult
  :straight t
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (register-preview--delay 0 register-preview-function #'consult-register-format)
  :config
  (advice-add #'register-preview :override #'consult-register-window)
  :general
  ("C-x b" #'consult-buffer
   "C-x 4 b" #'consult-buffer-other-window
   "M-g g" #'consult-goto-line
   "<help> a" #'consult-apropos
   "M-s r" #'consult-ripgrep
   "M-s u" #'consult-focus-lines
   "M-s i" #'consult-imenu
   "M-s l" #'consult-line
   "C-c f r" #'consult-recent-file))

;; ========================================================================================
;;; Terminals and Shells
;; ========================================================================================
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-copy-env "JULIA_DEPOT_PATH")
  (exec-path-from-shell-initialize))

(use-package vterm
  :straight t
  :if (memq window-system '(mac ns)))

(use-package eshell
  :straight (:type built-in)
  :custom
  (eshell-directory-name (concat power-local-dir "eshell/"))
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-kill-processes-on-exit t)
  (eshell-hist-ignoredups t)
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t)
  (eshell-term-name "xterm-256color"))

;;(add-to-list 'load-path (expand-file-name "modules/" power-emacs-dir))

;;(require 'power-defaults)


;; ========================================================================================
;;; Org Mode
;; ========================================================================================
(use-package org-journal
  :straight t
  :custom
  (org-journal-dir (expand-file-name "journal/" power-org-dir))
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%e %b %Y (%A)")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-format ""))

(use-package org
  :straight (:type built-in)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((dot . t)
                                 (emacs-lisp . t)
                                 (gnuplot . t)
                                 (latex . t)
                                 (octave . t)
                                 (python . t)))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-export-coding-system 'utf-8)
  (org-tags-column 0)
  (org-support-shift-select t)
  (org-directory power-org-dir)
  (org-agenda-files (list power-org-dir org-journal-dir))
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-use-speed-commands (lambda () (and (looking-at org-outline-regexp)
                                          (looking-back "%\*"))))
  (org-capture-templates `(("t" "Quick Task" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                            :immediate-finish t)
                           ("T" "Task" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                           ("d" "Templates for done tasks")
                           ("dt" "Quick Task" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* DONE %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                            :immediate-finish t)
                           ("dT" "Task" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* DONE %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                           ("n" "Quick Note" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                            :immediate-finish t)
                           ("r" "Note from Here" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n - %a")
                           ("N" "Note" entry
                            (file+headline ,power-org-main-file "Inbox")
                            "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))
  (org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QUES(q)"
                                 "|"
                                 "DONE(d!)" "DEFR(f)" "CANC(c)")
                       (sequence "TOREAD(t)" "READING(g)" "|" "READ(r)")))
  (org-todo-keyword-faces '(("TODO" . "red")
                            ("TOREAD" . "red")
                            ("PROG" . "goldenrod")
                            ("READING" . "goldenrod")
                            ("WAIT" . "orchid")
                            ("QUES" . "salmon")
                            ("DONE" . "forest green")
                            ("DEFR" . "steel blue")
                            ("CANC" . "dark gray")))
  :hook
  (org-mode . org-indent-mode)
  :diminish org-indent-mode
  :general
  ("C-c o a" #'org-agenda
   "C-c o c" #'org-capture
   "C-c o h" #'consult-org-agenda)
  (:keymaps 'org-mode-map
            "C-c r" #'org-refile ))
  

(use-package tex
  :straight auctex
  :hook
  (LaTeX-mode . reftex-mode)
  (reftex-toc-mode-hook . reftex-toc-rescan)
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  (LaTeX-mode . LaTeX-preview-setup)
  :config
  (use-package bibtex :straight t)
  (use-package pdf-tools :straight t)
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt))  preview-scale)))
  :custom
  (reftex-plug-intoAUCTex t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  (reftex-cite-format '((?a . "\\autocite[]{%l}")
                        (?b . "\\blockcquote[]{%l}{}")
                        (?c . "\\cite[]{%l}")
                        (?f . "\\footcite[]{%l}")
                        (?n . "\\nocite{%l}")
                        (?p . "\\parencite[]{%l}")
                        (?s . "\\smartcite[]{%l}")
                        (?t . "\\textcite[]{%l}")))
  (TeX-auto-save t)
  (TeX-byte-compile t)
  (TeX-clean-confirm nil)
  (TeX-fontify-script nil)
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 'color)
  (font-latex-fontify-script nil)
  (TeX-master 'dwim)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)
  (reftex-toc-split-windows-fraction 0.3)
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick nil)
  (pdf-annot-activate-created-annotations t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  :general
  (:keymaps 'TeX-mode-map
            "C-c c-o" #'TeX-recenter-output-buffer
            "C-c c-l" #'TeX-next-error
            "M-[" #'outline-previous-heading
            "M-]" #'outline-next-heading))

;; ========================================================================================
;;; Version Control
;; ========================================================================================
(use-package magit
  :straight t
  :hook
  (git-commit-mode . goto-address-mode)
  :init
  (setq-default magit-diff-refine-hunk t)
  :config
  (use-package magit-todos :straight t)
  :custom
  (transient-levels-file  (concat power-etc-dir "transient/levels"))
  (transient-values-file  (concat power-etc-dir "transient/values"))
  (transient-history-file (concat power-etc-dir "transient/history"))
  :general
  ("C-c g g" #'magit-status
   "C-c g s" #'magit-status))


;; ========================================================================================
;;; Programming
;; ========================================================================================
;;  Language Server Implementation
(use-package lsp-mode
  :straight t
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((c-mode . lsp)
   (c++-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :defer t
  :commands lsp-ui-mode)

;; C/C++ ----------------------------------------------------------------------------------
(use-package modern-cpp-font-lock
  :straight t
  :hook
  (after-init . (lambda () (modern-c++-font-lock-global-mode t)))
  (c-mode-common . (lambda () (c-set-style "stroustrup"))))

;; Clojure --------------------------------------------------------------------------------
(use-package clojure-mode
  :straight t
  :defer t
  :custom
  (nrepl-hide-special-buffers t)
  (nrepl-log-messages nil)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  (cider-overlays-use-font-lock t)
  (cider-prompt-for-symbol nil)
  (cider-repl-history-display-duplicates nil)
  (cider-repl-history-display-stile 'one-line)
  (cider-repl-history-file (concat power-cache-dir "cider-repl-history"))
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-quit-action 'delete-and-restore)
  (cider-repl-history-highlight-inserted-item t)
  (cider-repl-history-size 1000)
  (cider-repl-result-prefix ";; => ")
  (cider-repl-print-length 100)
  (cider-repl-user-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history nil)
  (cider-save-file-on-load t)
  (cider-stacktrace-default-filters '(tooling dup))
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  :config
  (use-package clojure-mode-extra-font-locking :straight t)
  (use-package cider :straight t))

;; Common Lisp ----------------------------------------------------------------------------
(use-package sly
  :straight t
  :defer t
  :custom
  (inferior-lips-program sbcl))

;; Data Markup ----------------------------------------------------------------------------
(use-package csv-mode
  :defer t
  :straight t)

(use-package json-mode
  :straight t
  :defer t)


;; Go -------------------------------------------------------------------------------------
(use-package go-mode
  :straight t)

;; Julia ----------------------------------------------------------------------------------
(use-package julia-mode
  :straight t
  :defer t)

(use-package julia-snail
  :straight t
  :after julia-mode
  :hook (julia-mode . julia-snail-mode))

;; Python ---------------------------------------------------------------------------------
(use-package python
  :straight (:type built-in)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3")))

(use-package blacken
  :straight t
  :after python
  :diminish blacken-mode
  :defer t
  :hook
  (python-mode . blacken-mode))

(use-package lsp-pyright
  :straight t
  :defer t
  :hook
  (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
  :custom
  (lsp-pyright-disable-language-service nil)
  (lsp-pyright-disable-organize-imports nil)
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-use-library-code-for-types t))


;; Racket ---------------------------------------------------------------------------------
(use-package racket-mode
  :straight t)

(provide 'init)
;;; init.el ends here
