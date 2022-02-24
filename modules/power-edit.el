;;; power-edit.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'format-all)
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-color-mode-line)

;; Configuration
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode)
            (diminish 'flycheck-mode)))
(setq flycheck-global-modes '(latex-mode)
      flycheck-emacs-lisp-load-path 'inherit
      flycheck-indication-mode (if (display-graphic-p) 'right-fringe
                                 'right-margin))


;; (use-package flycheck
;;   :defer t
;;   :diminish
;;   :hook
;;   (after-init . global-flycheck-mode)
;;   :commands (flycheck-add-mode)
;;   :custom
;;   (flycheck-global-modes '(python-mode latex-mode))
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;   (flycheck-indication-mode (if (display-graphic-p) 'right-fringe
;;                               'right-margin)))

;; (use-package flycheck-color-mode-line
;;   :defer t
;;   :config 
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(provide 'power-edit)
;;; power-edit.el ends here
