;;; power-keys.el --- -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

;; Keybinding utility
(use-package general
  :config
  (global-set-key (kbd power-leader-key) nil)
  (general-create-definer power-key-map
    :prefix power-leader-key))

;; Help with keybindings
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-mode +1)
  (which-key-setup-side-window-bottom)
  (setq which-key-idle-delay 0.3
        which-key-secondary-delay 0.3))

;; Better help
(use-package helpful
  :defer t)

(provide 'power-keys)
;;; power-keys.el ends here
