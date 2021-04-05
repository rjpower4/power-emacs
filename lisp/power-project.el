;;; power-project.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :init
  (setq projectile-cache-file (no-littering-expand-var-file-name "projectile.cache")
        projectile-auto-discover nil
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-known-projects-file (no-littering-expand-var-file-name
                                        "projectile.projects"))
  :config
  (projectile-mode +1))

(provide 'power-project)
;;; power-projectile.el ends here
