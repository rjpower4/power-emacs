;;; power-term.el -*- lexical-binding: t -*-

;; Desired packages
(straight-use-package 'exec-path-from-shell)
(if (not IS-WINDOWS)
    (straight-use-package 'vterm))

;; Configuration
(setq eshell-directory-name (concat power-local-dir "eshell/")
      eshell-scroll-to-bottom-on-input 'all
      eshell-scroll-to-bottom-on-output 'all
      eshell-kill-processes-on-exit t
      eshell-hist-ignoredups t
      eshell-glob-case-insensitive t
      eshell-error-if-no-glob t
      eshell-term-name "xterm-256color")

(exec-path-from-shell-copy-env "JULIA_DEPOT_PATH")
(when (or IS-MAC IS-LINUX IS-BSD)
  (exec-path-from-shell-initialize))

(provide 'power-term)
;;; power-term.el ends here
