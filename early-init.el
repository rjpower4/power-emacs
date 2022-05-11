;;; early-init.el --- loaded before the package system and GUI is initialized -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rolfe Power
;;
;; Author: Rolfe Power <rolfepower4@gmail.com>
;; Created: 23 Jan 2022
;;
;;; Code:

;; ------------------------------------------------------------------------------
;;; Garbage Collection
;; ------------------------------------------------------------------------------
(setq gc-cons-threshold most-positive-fixnum
      package-enable-at-startup nil
      frame-inhibit-implied-resize t
      inhibit-default-init t)

;; Reset to  8Mb
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 120 1024 1024))))

;; ------------------------------------------------------------------------------
;;; File loading
;; ------------------------------------------------------------------------------
(setq load-prefer-newer noninteractive)


(unless (daemonp)
  (defvar power--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (defun power/reset-file-handler-alist-h ()
    ;; Re-add so that changes since startup are preserved
    (dolist (handler file-name-handler-alist)
      (add-to-list 'power--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist power--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'power/reset-file-handler-alist-h))

;; ------------------------------------------------------------------------------
;;; UI
;; ------------------------------------------------------------------------------
(menu-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Welcome to the future
(set-language-environment "UTF-8")
(setq default-input-method nil)

(provide 'early-init)
;;; early-init.el ends here
