;;; power-project.el -*- lexical-binding: t; -*-

;; Desired Packages
(straight-use-package 'projectile)
(straight-use-package 'ibuffer-projectile)
(straight-use-package 'ripgrep)

;; Configuration
(add-hook 'after-init-hook #'projectile-mode)
(setq projectile-mode-line-prefix ""
      projectile-sort-order 'recentf
      projectile-enable-caching t
      projectile-use-git-grep t
      projectile-cache-file (concat power-cache-dir "projectile.cache")
      projectile-auto-discover nil
      projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
      projectile-known-projects-file (concat power-cache-dir 
                                             "projectile.projects"))
(add-hook #'ibuffer #'ibuffer-projectile-set-filter-groups)

(require 'projectile)
(def-projectile-commander-method ?r
  "Find file in project."
  (projectile-ripgrep))

;; Keymapping
(general-define-key
 :keymaps 'projectile-mode-map
 "C-x p" #'projectile-command-map)
(general-define-key
 "C-c p p" #'projectile-switch-project
 "C-c p r" #'projectile-ripgrep
 "C-c p f" #'projectile-find-file)



(provide 'power-project)
;;;  power-project.el ends here
