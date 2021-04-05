;;; power-org.el --- -*- lexical-binding: t -*-

;;; Commentary

;;; Code:

;; Org Mode
(use-package org
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-export-coding-system 'utf-8)
  (org-tags-column 0)
  (org-support-shift-se)
  (org-directory (concat (getenv "HOME") "/Documents/org"))
  (org-agenda-files (list org-directory
                          (expand-file-name "journal/" org-directory)))
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "%\*"))))
  (org-capture-templates '())
  (org-todo-keywords
        '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QUES(q)"
                    "|"
                    "DONE(d)" "DEFR(f)" "CANC(c)")))
  (org-todo-keyword-faces
        '(("TODO" . "red")
          ("PROG" . "goldenrod")
          ("WAIT" . "orchid")
          ("QUES" . "salmon")
          ("DONE" . "forest green")
          ("DEFR" . "steel blue")
          ("CANC" . "dark gray")))
  (org-archive-mark-done nil)
  (org-archive-location "%sarchive::* Archive")
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (eval-after-load 'org-indent
    (lambda () (diminish 'org-indent-mode))))

;; Babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (octave . t)
     (python . t)))) 

(use-package org-journal
  :after org
  :config
  (customize-set-variable 'org-journal-dir
                          (expand-file-name "journal/" org-directory))
  (customize-set-variable 'org-journal-file-format "%Y-%m-%d.org")
  (customize-set-variable 'org-jornal-date-format "%Y-%m-%d")
  (customize-set-variable 'org-journal-time-prefix "* ")
  (customize-set-variable 'org-journal-time-format ""))
(provide 'power-org)
;;; power-org.el ends here
