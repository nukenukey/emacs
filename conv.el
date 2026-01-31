;; -*- lexical-binding: t; -*-

(defun conv/text-input ()
  "pop up emacs window to edit text in then save to clipboard and close frame a la josh blais"
  (interactive)
  (let ((buf (get-buffer-create "*conv/text-input*")))
    (with-current-buffer buf
      (erase-buffer)
      (when (fboundp 'flyspell-mode)
        (flyspell-mode))
      (fset 'conv/text-input-finish
            (lambda ()
              (interactive)
              (clipboard-kill-region 1 (progn
                                         (end-of-buffer)
                                         (point-max)))
              (kill-buffer)
              (delete-frame)))
      (fset 'conv/text-input-never-mind
            (lambda ()
              (interactive)
              (erase-buffer)
              (kill-buffer)
              (delete-frame)))
      (keymap-local-set "C-c C-c" 'conv/text-input-finish)
      (keymap-local-set "C-c C-k" 'conv/text-input-never-mind)
      (setq mode-line-format " text input 😼 | C-c C-c to copy to clipboard and close | C-c C-k to erase and close"))
    (switch-to-buffer buf)))

(defun conv/package-upgrade ()
  "upgrades all the packages and asks you whether to upgrade for all"
  (interactive)
  (let (non-dep-packs (package--find-non-dependencies))
	(dolist (pack (package--upgradeable-packages))
	  (when (or
			 (member pack non-dep-packs)
			 (y-or-n-p (format "upgrade %s?" pack)))
		(with-demoted-errors "error encountered: %s" (package-upgrade pack)))))
  (message "done!"))

(defun conv/save-buffers-kill-terminal ()
  "runs conv/save-buffers-kill-terminal hook and then runs save-buffers-kill-terminal"
  (interactive)
  (run-hooks 'conv/save-buffers-kill-terminal-hook)
  (save-buffers-kill-terminal))

(add-hook 'conv/save-buffers-kill-terminal-hook '(lambda ()
												   (setq conv/last-buffer (buffer-name))))

(defun conv/switch-to-last-buffer ()
  "switches to conv/last-buffer"
  (switch-to-buffer conv/last-buffer))

(keymap-global-set "C-x C-c" 'conv/save-buffers-kill-terminal)

;; (defun conv/fortune-cowsay ()
;;   "gets scratch buffer and interts the output from `fortune -c | cowsay -r'"
;;   (interactive)
;;   (switch-to-buffer (get-scratch-buffer-create))
;;   (insert (shell-command-to-string "fortune -c | cowsay -r")))

(defun conv/org-agenda-list ()
  "convieniently sets up my org agenda list :>"
  (interactive)
  (org-agenda-list)
  (delete-other-windows))

(keymap-global-set "C-x j a" 'conv/org-agenda-list)

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
