;;; power-help.el -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'helpful)
(straight-use-package 'which-key)

;; Configuration
(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10
      which-key-idle-delay 0.3
      which-key-secondary-delay 0.3)

;; Keybinding
(general-define-key
 "C-h v" #'helpful-variable
 "C-h f" #'helpful-callable
 "C-h C-h" #'helpful-at-point
 "C-h k" #'helpful-key)

;; Enable
(add-hook 'after-init-hook
          (lambda ()
            (which-key-mode +1)
            (which-key-setup-side-window-bottom)
            (diminish 'which-key-mode)))

;; (use-package marginalia
;;   :init
;;   (marginalia-mode)
;;     ;; Refresh Selectrum on marginalia cycle
;;   (advice-add #'marginalia-cycle :after
;;               (lambda ()
;;                 (when (bound-and-true-p selectrum-mode)
;;    custom               (selectrum-exhibit 'keep-selected))))

;;   ;; Show Docs
;;   (setq marginalia-annotators
;;         '(marginalia-annotators-heavy
;;           marginalia-annotators-light
;;           nil)))


(provide 'power-help)
;;; power-help.el ends here
