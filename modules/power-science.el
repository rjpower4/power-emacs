;;; power-science.el -*- lexical-binding: t; -*-

;; Desired Packages
(straight-use-package 'julia-mode)
(straight-use-package 'julia-snail)
(straight-use-package 'csv-mode)
(straight-use-package 'json-mode)

;; Configuration
(add-hook #'julia-mode-hook #'julia-snail-mode)

(provide 'power-science)
