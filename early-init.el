
;;; early-init.el --- -*- lexical-binding: t; -*-

;; Garbage collection slows down start up, so we increase
;; the threshold temporarily to stop it.
(setq gc-cons-threshold most-positive-fixnum)

;; Emacs 27+ performs package initialization before the `user-init-file'
;; and after the `early-init-file', so turn it off here as we'll
;; do it
(setq package-enable-at-startup nil)

;; `file-name-handler-alist' is used on every `require', `load', and more.
;; We no-op this.
(unless (daemonp)
  (defvar power--initial-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (defun power/reset-file-handler-alist-h ()
    ;; Re-add so that changes since startup are preserved
    (dolist (handler file-name-handler-alist)
      (add-to-list 'power--initial-file-name-handler-alist handler))
    (setq file-name-handler-alist power--initial-file-name-handler-alist))
  (add-hook 'emacs-startup-hook #'power/reset-file-handler-alist-h))

(provide 'early-init)
;;; early-init.el ends here

