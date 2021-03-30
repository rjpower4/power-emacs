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

;;; Package setup
(require 'power-package)

;;; Basic Config
(require 'power-core)
(require 'power-keys)
(require 'power-crux)
(require 'power-dired)

;;; Appearance
(require 'power-themes)

;;; Tools
(require 'power-org)
(require 'power-search)
(require 'power-project)
(require 'power-movement)
(require 'power-term)
(require 'power-company)
(require 'power-git)

;;; Languages
(require 'power-latex)
(require 'power-julia)
(require 'power-clojure)

;;
;;; UX Packages
;; Smartparens
(use-package smartparens
  :diminish
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (require 'smartparens-latex)
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
;;; Useful functions
(defun power/where-am-i ()
  "Tell me where I am."
  (interactive)
  (message (kill-new (if (buffer-file-name) (buffer-file-name) (buffer-name)))))

;;
;;; Key Bindings

(general-define-key
 "<f5>" #'power/switch-theme
 "M-n" #'forward-paragraph
 "M-p" #'backward-paragraph
 "C-h f" #'helpful-callable
 "C-h v" #'helpful-variable
 "C-h k" #'helpful-key
 "C-=" #'power/text-scale-default
 "C-+" #'text-scale-increase
 "C--" #'text-scale-decrease)

(power-key-map
  "M-SPC" '(consult-line :wk "find in buffer")
  
  "h"   '(:ignore t :which-key "help")
  "h f" '(helpful-callable :which-key "function")
  "h v" '(helpful-variable :wk "variable")
  "h k" '(helpful-key :wk "key")
  "h a" '(consult-apropos :wk "apropos")
  "h m" '(describe-mode :wk "mode")
  "h M" '(consult-man :wk "man")

  "w"   '(:ignore t :wk "window")
  "w v" '(split-window-right :wk "vertical split")
  "w s" '(split-window-below :wk "horizontal split")
  "w w" '(ace-window :wk "ace-window")
  "w d" '(delete-window :wk "delete")
  "w u" '(winner-undo :wk "winner undo")

  "b"   '(:ignore t :wk "buffer")
  "b b" '(consult-buffer :wk "switch")
  "b i" '(ibuffer :wk "ibuffer")
  "b d" '(kill-current-buffer :wk "delete")
  "b D" '(kill-buffer-and-window :wk "delete with window")

  "f"   '(:ignore t :wk "file")
  "f f" '(find-file :wk "find file")
  "f r" '(consult-recent-file :wk "recentf")
  "f s" '(save-buffer :wk "save buffer")
  "f i" '(crux-find-user-init-file :wk "open init")

  "s"   '(:ignore t :wk "search")
  "s s" '(consult-line :wk "buffer")
  "s d" '(consult-ripgrep :wk "directory")

  "j"   '(:ignore t :wk "jump")
  "j c" '(avy-goto-char-2 :wk "char")
  "j d" '(dired-jump :wk "dired")
  "j d" '(dired-jump-other-window :wk "dired other window")
  "j l" '(avy-goto-line :wk "line")
  "j L" '(consult-goto-line :wk "line by number")
  "j w" '(avy-goto-word-1 :wk "word")

  "g"   '(:ignore t :wk "git")
  "g s" '(magit-status :wk "status")

  "t"   '(:ignore t :wk "toggle")
  "t t" '(power/switch-theme :wk "theme")

  "p"   '(:ignore t :wk "project")
  "p f" '(project-find-file :wk "find file")
  
  "o"   '(:ignore t :which-key "open")
  "o a" '(org-agenda :wk "agenda")
  "o t" '(vterm :which-key "vterm")
  "o e" '(eshell :which-key "eshell")
  "o -" '(dired-jump :wk "dired"))

(provide 'init)
