;;; power-git.el --- -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(use-package magit
  :config
  (setq-default magit-diff-refine-hunk t)
  (use-package magit-todos)
  (use-package fullframe)
  (with-eval-after-load 'magit
    (fullframe magit-status magit-mode-quit-window))
  (require 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))
  
(provide 'power-git)
;;; power-git.el ends here
