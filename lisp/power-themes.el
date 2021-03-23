;;; power-themes.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Icons
(use-package all-the-icons)

;; External theme packages
(use-package color-theme-sanityinc-solarized)
(use-package color-theme-sanityinc-tomorrow)
(use-package modus-themes)

(defvar power-light-theme 'modus-operandi)
(defvar power-dark-theme  'modus-vivendi)
(load-theme power-light-theme t)

(defvar power-quick-switch-themes
  (let ((themes-list
         (list power-light-theme
               power-dark-theme)))
    (nconc themes-list themes-list)))

(defun power/switch-theme ()
  "Switch between themes."
  (interactive)
  (if-let* ((next-theme (cadr power-quick-switch-themes)))
      (progn
        (when-let* ((current-theme (car power-quick-switch-themes)))
          (disable-theme (car power-quick-switch-themes)))
        (load-theme next-theme t)
        (message "Loaded theme: %s" next-theme)))
  (setq power-quick-switch-themes (cdr power-quick-switch-themes)))

(provide 'power-themes)
;;; power-themes.el ends here
