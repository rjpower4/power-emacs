;;; power-selection.el -*- lexical-binding: t; -*-

;; Desired  packages
(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'embark)
(straight-use-package 'company)

;; Configuration
(customize-set-variable 'vertico-cycle t)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(setq completion-category-defaults nil)
(setq marginalia-annotators
      '(marginalia-annotators-heavy marginalia-annotators-light nil))
(setq completion-in-region-function #'consult-completion-in-region)
(setq register-preview--delay 0
      register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)
(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'after-init-hook
          (lambda ()
            (global-company-mode)
            (diminish 'company-mode)))

;; Keybinding
(general-define-key
 "C-." #'embark-act
 "C-x b" #'consult-buffer
 "C-x 4 b" #'consult-buffer-other-window
 "M-g g" #'consult-goto-line
 "<help> a" #'consult-apropos
 "M-s r" #'consult-ripgrep
 "M-s u" #'consult-focus-lines
 "M-s i" #'consult-imenu
 "M-s l" #'consult-line)

;; Enable
(add-hook 'after-init-hook #'vertico-mode)
(add-hook 'after-init-hook #'marginalia-mode)


;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package embark
;;   :general
;;   ("C-." 'embark-act)
;;   ("C-;" 'embark-dwim)  
;;   ("C-h B" 'embark-bindings)

;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

(provide 'power-selection)
