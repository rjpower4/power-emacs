;;; power-vcs.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'magit)
(straight-use-package 'magit-todos)
(straight-use-package 'forge)
(straight-use-package 'treemacs-magit)

;; Configuration

(setq transient-levels-file  (concat power-etc-dir "transient/levels")
      transient-values-file  (concat power-etc-dir "transient/values")
      transient-history-file (concat power-etc-dir "transient/history"))
(setq-default magit-diff-refine-hunk t)
(add-hook 'git-commit-mode-hook 'goto-address-mode)
(setq forge-database-file (concat power-cache-dir "forge-database.sqlite"))

(provide 'power-vcs)
