;; -*- lexical-binding: t; -*-
(defun conv/cornell-split-file (file-name)
  "returns a list consisting of the name of a file before the extension and the extension. If file-name does not contain a `.', returns nil"
  (s-index-of "." file-name)
  `(,(substring file-name 0 (s-index-of "." file-name))
	,(substring file-name (s-index-of "." file-name))))

(defun conv/cornell-init ()
  "makes windows for cornell style notes"
  (interactive)
  (if (not (f-this-file))
	  (message "this buffer is not a file?")
	(delete-other-windows)
	(split-window-horizontally 120)
	(other-window 1)
	(let '(conv/cornell-buffer-name-list (conv/cornell-split-file (buffer-name)))
	  (find-file (concat (car conv/cornell-buffer-name-list) "-cues" (car (cdr conv/cornell-buffer-name-list)))))))
