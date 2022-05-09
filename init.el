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

(defvar power-gc-cons-threshold (* 128 (* 1024 1024)))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold power-gc-cons-threshold)))

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
(defconst power-local-dir (concat power-emacs-dir ".local/"))
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
;;; Modules Path
;; ========================================================================================
(add-to-list 'load-path (expand-file-name "modules/" power-emacs-dir))

(require 'power-defaults)

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
;;; Modules
;; ========================================================================================
(require 'power-core)
(require 'power-themes)
(require 'power-help)
(require 'power-selection)
(require 'power-term)

(require 'power-edit)
(require 'power-filesystem)
(require 'power-base-devel)
(require 'power-org)
(require 'power-publishing)
(require 'power-science)
(require 'power-lisp)
(require 'power-vcs)
(require 'power-project)



(provide 'init)
;;; init.el ends here
