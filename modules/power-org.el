;;; power-org.el -*- lexical-binding: t -*-

;; Desired Packages
(straight-use-package 'org-ref)
(straight-use-package 'org-journal)

;; Configuration
(setq org-journal-dir (expand-file-name "journal/" power-org-dir)
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%e %b %Y (%A)"
      org-journal-date-prefix "#+TITLE: "
      org-journal-time-format "")
(setq power-org-main-file (concat power-org-dir "homebase.org"))
(setq org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-export-coding-system 'utf-8
      org-tags-column 0
      org-support-shift-select t
      org-directory power-org-dir
      org-agenda-files (list power-org-dir
                             org-journal-dir)
      org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
      org-use-speed-commands (lambda ()
                               (and (looking-at org-outline-regexp)
                                    (looking-back "%\*")))
      org-capture-templates `(("t" "Quick Task" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                               :immediate-finish t)
                              ("T" "Task" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* TODO %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                              ("d" "Templates for done tasks")
                              ("dt" "Quick Task" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* DONE %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                               :immediate-finish t)
                              ("dT" "Task" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* DONE %^{Task}\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
                              ("n" "Quick Note" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
                               :immediate-finish t)
                              ("r" "Note from Here" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n - %a")
                              ("N" "Note" entry
                               (file+headline ,power-org-main-file "Inbox")
                               "* %^{Note}\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))
      org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "QUES(q)"
                                    "|"
                                    "DONE(d!)" "DEFR(f)" "CANC(c)")
                          (sequence "TOREAD(t)" "READING(g)" "|" "READ(r)"))
      org-todo-keyword-faces '(("TODO" . "red")
                               ("TOREAD" . "red")
                               ("PROG" . "goldenrod")
                               ("READING" . "goldenrod")
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

;; Functions
(defun power/find-org-homebase ()
  "Open the homebase org file."
  (interactive)
  (find-file (concat power-org-dir "homebase.org")))

;; Keybindings
(power-open-def
  "a" #'org-agenda
  "c" #'org-capture
  "h" #'consult-org-agenda
  "H" '(power/find-org-homebase :which-key "home"))

(general-define-key
 :keymaps 'org-mode-map
 "C-c ]" #'org-ref-insert-link
 "C-c r" #'org-refile)

(provide 'power-org)
;;; power-org.el ends here
