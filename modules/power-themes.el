;;; power-themes.el  -*- lexical-binding: t; -*-

;; Desired packages
(straight-use-package 'modus-themes)
(straight-use-package 'gruvbox-theme)
(straight-use-package 'all-the-icons)

;; Configuration
(setq modus-themes-bold-constructs t
      modus-themes-completions 'nil
      modus-themes-no-mixed-fonts t
      modus-themes-diffs 'desaturated
      modus-themes-fringes 'intense
      modus-themes-lang-checkers 'colored-background
      modus-themes-links nil
      modus-themes-mail-citations nil
      modus-themes-mode-line nil
      modus-themes-org-blocks nil
      modus-themes-paren-match '(intense bold)
      modus-themes-prompts '(intense gray)
      modus-themes-region '(bg-only no-extend)
      modus-themes-scale-headings nil
      modus-themes-syntax '(yellow-comments green-strings)
      modus-themes-variable-pitch-headings nil
      modus-themes-variable-pitch-ui nil
      modus-themes-slanted-constructs t)

;; Load desired theme
(load-theme 'modus-vivendi t)

(provide 'power-themes)
;;; power-themes.el ends here
