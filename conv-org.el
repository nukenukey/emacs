;; -*- lexical-binding: t; -*-
(defun conv/org-agenda-list ()
  "convieniently sets up my org agenda list :>"
  (interactive)
  (org-agenda-list)
  (delete-other-windows)
  (org-agenda-day-view))
