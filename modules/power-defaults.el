;;; power-defaults.el -*- lexical-binding: t; -*-

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

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

(when IS-WINDOWS
  (set-face-attribute 'default nil :font "JuliaMono-10")
  (set-face-attribute 'fixed-pitch nil :font "JuliaMono-10"))

(setq gnutls-verify-error t)

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

(add-hook 'after-init-hook
          (lambda () (display-time-mode 1)))
(setq display-time-24hr-format t
      display-time-day-and-date t)

;; ----------------------------------------------------------------------------------------
;; History
;; ----------------------------------------------------------------------------------------
(add-hook 'after-init-hook 'recentf-mode)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'after-init-hook 'save-place-mode)

(setq recentf-save-file (concat power-cache-dir "recentf")
      recentf-auto-cleanup 'never
      recentf-max-menu-items 0
      recentf-max-saved-items 300)
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude power-etc-dir)
  (add-to-list 'recentf-exclude power-cache-dir)
  (add-to-list 'recentf-exclude power-local-dir))

(setq enable-recursive-minibuffers t
      history-length 1000
      savehist-additional-variables '(mark-ring
                                       global-mark-ring
                                       search-ring
                                       regexp-search-ring
                                       extended-command-history)
      savehist-autosave-interval 300
      savehist-file (concat power-cache-dir "history"))

(setq save-place-limit 500
      save-place-file (concat power-cache-dir "places"))

;; ----------------------------------------------------------------------------------------
;; Window Management
;; ----------------------------------------------------------------------------------------
(defvar winner-dont-bind-my-keys)
(setq winner-boring-buffers
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
(add-hook 'after-init-hook (lambda () (winner-mode 1)))

;; ----------------------------------------------------------------------------------------
;; Spelling
;; ----------------------------------------------------------------------------------------
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

;; ----------------------------------------------------------------------------------------
;; Parentheses
;; ----------------------------------------------------------------------------------------
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(add-hook 'after-init-hook #'show-paren-mode)
(add-hook 'after-init-hook #'electric-pair-mode)

;; ----------------------------------------------------------------------------------------
;; Files
;; ----------------------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-so-long-mode)
(setq so-long-threshold 400)

;; ----------------------------------------------------------------------------------------
;; Dired
;; ----------------------------------------------------------------------------------------
(global-auto-revert-mode t)                    ; Enable auto-reverting
(put 'dired-find-alternate-file 'disabled nil) ; Reuse same dired buffer
(setq dired-listing-switches "-alh"
      dired-dwim-target t
      dired-recursive-deletes 'top        ; Ask about recursive deletes/copies
      dired-recursive-copies  'top        ; at top level only

      ;; Auto refresh dired but don't brag about it
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil

      delete-by-moving-to-trash t         ; Separation anxiety
      load-prefer-newer t                 ; Get newest

      auto-revert-use-notify nil
      auto-revert-interval 3)              ; 3 seconds

;; Use '-' to go up a directory
(add-hook #'dired-mode
          (lambda () (local-set-key (kbd "-") (lambda () (interactive)
                                                (find-alternate-file "..")))))

(provide 'power-defaults)
