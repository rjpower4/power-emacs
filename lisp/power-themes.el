;;; power-themes.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Icons
(use-package all-the-icons)

;; Fonts
(set-face-attribute 'default nil :height 130)

;; External theme packages
(use-package doom-themes)
(use-package color-theme-sanityinc-solarized)
(use-package color-theme-sanityinc-tomorrow)
(use-package modus-themes
  :config
  (setq modus-themes-no-mixed-fonts t
        modus-themes-org-blocks 'grayscale
        modus-themes-slanted-constructs t))

;; Set location for sunrise/sunset
(setq calendar-latitude 40.424660)
(setq calendar-longitude -86.848460)

(defvar power-light-theme 'modus-operandi)
(defvar power-dark-theme  'modus-vivendi)

(defun power/night-mode ()
  "Set the theme to a dark mode"
  (interactive)
  (consult-theme power-dark-theme))

(defun power/day-mode ()
  "Set the theme to a light mode"
  (interactive)
  (consult-theme power-light-theme))

(defun power/auto-theme ()
  "Select the theme automatically based on the time of day."
  (require 'solar)
  (let* ((cur-hour (string-to-number (substring (current-time-string) 11 13)))
         (sun-events (solar-sunrise-sunset (calendar-current-date)))
         (sunrise (caar sun-events))
         (sunset (caadr sun-events)))
    (if (and (> cur-hour sunrise) (< cur-hour sunset))
        (power/day-mode)
      (power/night-mode))))

(power/auto-theme)

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
        (consult-theme next-theme)))
  (setq power-quick-switch-themes (cdr power-quick-switch-themes)))

(provide 'power-themes)
;;; power-themes.el ends here
