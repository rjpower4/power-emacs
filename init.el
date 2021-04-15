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
  (load "early-init"
	    nil t))

;; Benchmark startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time
                                                        before-init-time)))
                     gcs-done)))

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
(defconst EMACS27+   (> emacs-major-version 26))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkely-unix)))
(defconst power-leader-key "C-c" "Leader prefix key")

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
      gcmh-verbose nil)
(setq idle-update-delay 1.0)
(setq redisplay-skip-fontification-on-input t)
(unless IS-LINUX (setq command-line-x-option-alist nil))
(unless IS-MAC   (setq command-line-ns-option-alist nil))

;;; Package setup
(require 'power-package)

;; Optimizations
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)

;;; Basic Config
(require 'power-core)
(require 'power-keys)
(require 'power-crux)
(require 'power-dired)

;;; Tools
(require 'power-org)
(require 'power-search)
(require 'power-project)
(require 'power-movement)
(require 'power-term)
(require 'power-company)
(require 'power-git)
(require 'power-elfeed)
(require 'power-irc)
(require 'power-fly)
(require 'power-snippets)
(require 'power-lsp)

;;; Appearance
(require 'power-themes)


;;; Languages
(require 'power-data)
(require 'power-latex)
(require 'power-julia)
(require 'power-clojure)
(require 'power-python)
(require 'power-rust)

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

(general-create-definer window-definer
  :prefix "C-x w"
  :wk "window")

(window-definer
  "v" '(split-window-right :wk "vertical split")
  "s" '(split-window-below :wk "horizontal split")
  "w" '(ace-window :wk "ace-window")
  "d" '(delete-window :wk "delete")
  "u" '(winner-undo :wk "winner undo")
  "o" '(other-window :wk "other window"))

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

(general-create-definer buffer-definer
  :prefix "C-c b")

(buffer-definer
 ""  '(:ignore t :wk "buffer")
 "b" '(consult-buffer :wk "switch")
 "i" '(ibuffer :wk "ibuffer")
 "d" '(kill-current-buffer :wk "delete")
 "D" '(kill-buffer-and-window :wk "delete with window"))

(general-create-definer file-definer
  :prefix "C-c f")

(file-definer
  ""  '(:ignore t :wk "file")
  "f" '(find-file :wk "find file")
  "r" '(consult-recent-file :wk "recentf")
  "s" '(save-buffer :wk "save buffer")
  "i" '(crux-find-user-init-file :wk "open init"))

(general-create-definer search-definer
  :prefix "C-c s")

(search-definer
  ""  '(:ignore t :wk "search")
  "s" '(consult-line :wk "buffer")
  "d" '(consult-ripgrep :wk "directory"))


(general-create-definer jump-definer
  :prefix "C-c j")

(jump-definer
  ""  '(:ignore t :wk "jump")
  "c" '(avy-goto-char-2 :wk "char")
  "d" '(dired-jump :wk "dired")
  "d" '(dired-jump-other-window :wk "dired other window")
  "l" '(avy-goto-line :wk "line")
  "L" '(consult-goto-line :wk "line by number")
  "w" '(avy-goto-word-1 :wk "word")
  "b" '(power/switch-to-buffer :wk "buffer across frames"))

(general-create-definer git-definer
  :prefix "C-c g")

(git-definer
 ""  '(:ignore t :wk "git")
 "s" '(magit-status :wk "status"))

(general-create-definer toggle-definer
  :prefix "C-c t")

(toggle-definer
 ""  '(:ignore t :wk "toggle")
 "t" '(power/switch-theme :wk "theme"))

(general-create-definer project-definer
  :prefix "C-c p")

(project-definer
 ""  '(:ignore t :wk "project")
 "f" '(project-find-file :wk "find file"))

(general-create-definer open-definer
  :prefix "C-c o")

(open-definer
  ""   '(:ignore t :which-key "open")
  "a"  '(org-agenda :wk "agenda")
  "t"  '(vterm :which-key "vterm")
  "e"  '(eshell :which-key "eshell")
  "E"  '(elfeed :wk "elfeed")
  "i"  '(circe :wk "irc")
  "-"  '(dired-jump :wk "dired"))

(power-key-map
  "SPC" '(consult-line :wk "find in buffer"))



(provide 'init)
