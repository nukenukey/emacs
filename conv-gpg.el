;; -*- lexical-binding: t; -*-
(defun conv/gpg-detach-sign-file (file)
  "function to detach-sign files using gpg bc epa doesn't gel w/ me"
  (interactive "b")
  (if (file-exists-p file)
      (progn (async-shell-command (concat "gpg --detach-sign " file))
             (with-current-buffer "*Async Shell Command*"
               (local-set-key "q" 'kill-buffer-and-window))
             (other-window 1))
    (error "file %s does not exist" file)))

(defun conv/gpg-verify-file (file)
  "function to verify files using gpg bc epa still doesn't gel w/ me"
  (interactive "f")
  (if (file-exists-p file)
      (progn
        (async-shell-command (concat "gpg --verify " file))
        (local-set-key "q" 'kill-buffer-and-window)
        (other-window 1)
        (message "gpg process completed"))
    (error "file %s does not exist" file)))
