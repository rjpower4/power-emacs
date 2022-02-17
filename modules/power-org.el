;;; power-org.el -*- lexical-binding: t -*-

;; Desired Packages
(straight-use-package 'org-ref)

;; Configuration
(setq org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-export-coding-system 'utf-8
      org-tags-column 0
      org-support-shift-select t
      org-directory power-org-dir
      org-agenda-files (list power-org-dir
                             (concat power-org-dir "research/")
                             (concat power-org-dir "research/projects/"))
      org-use-speed-commands (lambda ()
                               (and (looking-at org-outline-regexp)
                                    (looking-back "%\*")))
      org-capture-templates '()
      org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QUES(q)"
                                    "|"
                                    "DONE(d)" "DEFR(f)" "CANC(c)"))
      org-todo-keyword-faces '(("TODO" . "red")
                               ("PROG" . "goldenrod")
                               ("WAIT" . "orchid")
                               ("QUES" . "salmon")
                               ("DONE" . "forest green")
                               ("DEFR" . "steel blue")
                               ("CANC" . "dark gray")))
(add-hook #'org-mode-hook #'org-indent-mode)
(with-eval-after-load #'org-indent
  (lambda () (diminish 'org-indent-mode)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (octave . t)
   (python . t)))

;; Keybindings
(general-define-key
 :keymaps 'org-mode-map
 "C-c ]" 'org-ref-insert-link)

(provide 'power-org)
;;; power-org.el ends here
