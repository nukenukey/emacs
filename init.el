;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025 aug <baleofhay@proton.me>

(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

(use-package use-package
  :init
  (setq use-package-check-before-init t
		use-package-always-defer nil))

(use-package package
  :defer t
  :config
  (setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
						   ("org" . "https://orgmode.org/elpa/")
						   ("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

(use-package tetris
  :commands
  (tetris)
  :defer t
  :config
  (bind-key "w" 'tetris-rotate-prev tetris-mode-map)
  (bind-key "a" 'tetris-move-left tetris-mode-map)
  (bind-key "s" 'tetris-move-down tetris-mode-map)
  (bind-key "d" 'tetris-move-right tetris-mode-map)
  (bind-key "m" 'tetris-move-bottom tetris-mode-map))

(use-package dired
  :commands
  (dired dired-jump)
  :defer t
  :config
  (setq dired-listing-switches "-Alh")
  (bind-key "b" 'dired-view-file dired-mode-map)
  (unbind-key "v" dired-mode-map)
  (bind-key "v" 'vterm dired-mode-map)
  (unbind-key "e" dired-mode-map)
  (bind-key "e" 'eshell dired-mode-map)
  (bind-key "-" 'dired-up-directory dired-mode-map))

(use-package org
  ;; :commands
  ;; (org-mode org-agenda-list org-agenda)
  :defer t
  ;; :init
  ;; (add-hook 'org-mode-hook (lambda ()
  ;; 							 (setq right-margin-width 20 left-margin-width 20)))
  :config
  (setq org-agenda-files '("~/org/agenda")
        diary-file "~/.emacs.d/diary.gpg"
        org-html-validation-link nil
		org-agenda-start-with-log-mode t
		org-log-into-drawer t
		org-hide-leading-stars t
		org-return-follows-link t
		org-agenda-span 'day
		org-html-style (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (getenv "HOME") "/org/org-style.css\">")
		org-export-with-section-numbers nil
		org-export-with-toc nil
		org-export-dispatch-use-expert-ui t
		org-todo-keywords '((sequence "TODO(t)" "TOTURNIN(m)" "CURRENT(c)" "URGENT(u)" "DEFERRED(f)" "ASSIGNMENT(a)" "|" "DONE(d)" "NOTDOING(n)"))))

(use-package tramp
  :defer t
  :init
  (setq tramp-use-scp-direct-remote-copying t) ;; for speed

  ;; do not auto-save if using sudo
  (connection-local-set-profile-variables
   'my-auto-save-profile
   '((buffer-auto-save-file-name . nil)))
  (connection-local-set-profiles
   '(:application tramp :protocol "sudo")
   'my-auto-save-profile)

  :bind
  ("C-x M-r" . 'tramp-cleanup-all-buffers))

(use-package vc
  :defer t
  :init
  (setq vc-handled-backends '(Git)))

(use-package windmove
  :defer nil
  :bind
  ("M-H" . 'windmove-left)
  ("M-J" . 'windmove-down)
  ("M-K" . 'windmove-up)
  ("M-L" . 'windmove-right))

(use-package display-line-numbers
  :defer nil
  :init
  (column-number-mode)
  (setq display-line-numbers-width-start t)
  (global-display-line-numbers-mode t)
  (dolist (mode '(term-mode-hook ;; not in some modes please
				  shell-mode-hook
                  vterm-mode-hook
				  eshell-mode-hook
                  tldr-mode-hook
				  ;; org-mode-hook
				  doc-view-mode-hook
	  			  fireplace-mode-hook))
    (add-hook mode (lambda ()
                     (display-line-numbers-mode 0)))))

(use-package hl-line
  :defer nil
  :init
  (global-hl-line-mode)
  (keymap-global-set "M-p" '(lambda ()
                              (interactive)
                              (hl-line-mode 'toggle)))
  (dolist (mode '(vterm-mode-hook
                  dashboard-mode-hook))
    (add-hook mode (lambda ()
                     (hl-line-mode 'toggle)))))

(use-package tab-bar
  :commands
  (tab-bar-new-tab)
  :defer t
  :config
  (setq tab-bar-format nil)
  (keymap-set tab-prefix-map "l" #'(lambda ()
                                     (interactive)
                                     (message (format "num of tabs: %s" (length (tab-bar-tabs))))))
  (keymap-set tab-prefix-map "0" #'(lambda ()
									 (interactive)
									 (tab-close)
									 (when (= (length (tab-bar-tabs)) 1)
									   (tab-bar-mode 0))))
  (keymap-set tab-prefix-map "1" #'(lambda ()
									 (interactive)
									 (tab-close-other)
									 (tab-bar-mode 0)))
  (keymap-global-set "C-S-t" 'tab-bar-undo-close-tab))

(use-package compile
  :defer nil
  :init
  (add-hook 'c++-mode-hook (lambda ()
							 (setq-local compile-command (concat "g++ -fdiagnostics-all-candidates -fsanitize=address -Wall -Wextra -Werror -Wpedantic -g " (f-filename (f-this-file)) " -o " (substring (f-filename (f-this-file)) 0 (s-index-of "." (f-filename (f-this-file))))))))
  (add-hook 'java-mode-hook (lambda ()
							  (setq-local compile-command (concat "javac " (f-filename (f-this-file))))))
  (add-hook 'rust-mode-hook (lambda ()
							  (setq-local compile-command (concat "rustc " (f-filename (f-this-file))))))
  (add-hook 'c-mode-hook (lambda ()
						   (setq-local compile-command (concat "gcc -fsanitize=address -Wall -Wextra -Werror -Wpedantic -g " (f-filename (f-this-file)) " -o " (substring (f-filename (f-this-file)) 0 (s-index-of "." (f-filename (f-this-file)))))))))

(use-package epa
  :defer t
  :config
  (setq epa-keys-select-method 'minibuffer))

(use-package recentf
  :defer nil ;; I will always want this available
  :bind
  ("C-x j r" . 'counsel-recentf)
  ("C-x j C-r" . 'recentf-save-list)
  ("C-x j M-r" . 'recentf-cleanup)
  :init
  (setq recentf-exclude '("^~/org/agenda/.*$" "^.*~$" "^~/.emacs.d/games/tetris-scores$" "^.*#$")
		recentf-max-saved-items 64
		recentf-auto-cleanup 'mode)
  (add-to-list 'auto-save-hook #'recentf-save-list)
  :config
  (recentf-mode))

(use-package emacs
  :defer nil
  :bind
  ("C-S-w" . 'clipboard-kill-region)
  ("M-W" . 'clipboard-kill-ring-save)

  ("C-=" . 'text-scale-increase)
  ("C--" . 'text-scale-decrease)
  ("C-x f" . 'query-replace)
  ("C-x h" . 'previous-buffer)
  ("C-x l" . 'next-buffer)

  ("C-x C-l l" . 'count-lines-page)
  ("C-x C-l p" . 'check-parens)

  ("C-k" . 'kill-whole-line)
  ("C-x C-a" . 'mark-whole-buffer)
  ("M-D" . 'backward-kill-word)

  ("s-x" . 'counsel-linux-app)

  ("M-/" . 'hippie-expand)
  ("C-x C-b" . 'electric-buffer-list)
  ("C-x b" . 'counsel-switch-buffer)

  ("C-x C-r" . 'tramp-revert-buffer-with-sudo)
  ("C-x M-f" . 'find-file-other-window)
  ("M-k" . 'kill-line)

  ("C-x j d s" . desktop-save)
  ("C-x j d r" . desktop-read)
  ("C-x j d c" . desktop-clear)

  ("C-x j u" . 'compile)

  ("C-x j c" . 'conv/cornell-init)
  ;; ("C-x j a" . 'org-agenda-list)

  ("C-x j l" . 'lsp)
  ("C-x j M-l" . 'lsp-workspace-shutdown)
  ("C-x j f" . 'flyspell-buffer)
  ("C-x j C-f" . 'flyspell-mode)

  ("C-x j m" . 'make-directory)

  :config
  (keymap-set help-map "g" 'shortdoc-display-group)
  (unbind-key "C-x C-l")

  (setq	tab-width 4)

  (put 'narrow-to-region 'disabled nil)
  (setq hippie-expand-try-functions-list
		'(try-complete-file-name-partially try-complete-file-name
										   try-expand-all-abbrevs
										   try-expand-dabbrev
										   try-expand-dabbrev-from-kill
										   try-expand-dabbrev-all-buffers
										   try-complete-lisp-symbol-partially
										   try-complete-lisp-symbol
										   try-expand-list try-expand-line))

  (if (string= (getenv "XDG_CURRENT_DESKTOP") "sway")
	  (setq conv/sway t)
	(setq conv/sway nil))
  (load-file (concat user-emacs-directory "third-party.el"))
  (load-file (concat user-emacs-directory "local.el"))
  (load-file (concat user-emacs-directory "conv.el"))

  (setq initial-scratch-message nil)
  
  (add-hook 'text-mode-hook 'flyspell-mode)

  (setq scroll-conservatively 100
		shell-command-prompt-show-cwd t)
  (setq-default cursor-type 'bar)

  (scroll-bar-mode -1)
  (setq image-scaling-factor 1.0)

  (electric-pair-mode)
  (electric-indent-mode)
  ;; (desktop-save-mode 1)

  (setq-default tab-stop-list 4
				indent-tab-modes nil
				tab-always-indent nil
				indent-tabs-mode t
				tab-width 4
				c-basic-offset 4)

  ;; (setq indent-line-function 'insert-tab
  (setq split-width-threshold 1)
  (defvaralias 'c-basic-offset 'tab-width)

  (keymap-global-set "M-<up>" '(lambda ()
								 (interactive)
								 (kill-whole-line)
								 (previous-line)
								 (yank)
								 (previous-line)))
  (keymap-global-set "M-<down>" '(lambda ()
								   (interactive)
								   (kill-whole-line)
								   (next-line)
								   (yank)
								   (previous-line)))
  (keymap-global-set "M-o" '(lambda ()
							  "insert a newline at the beginning of the line and move back one line"
							  (interactive)
							  (move-beginning-of-line 1)
							  (newline)
							  (backward-char)))
  (keymap-global-set "C-M-o" '(lambda ()
								"insert a newline before the current line and stay where you were"
								(interactive)
								(let ((poi (point)))
								  (beginning-of-line)
								  (newline)
								  (goto-char (+ 1 poi)))))
  (keymap-global-set "C-o" '(lambda ()
							  "move to the end of the current line and insert a newline"
							  (interactive)
							  (end-of-line)
							  (electric-newline-and-maybe-indent)))
  (keymap-global-set "M-[" '(lambda ()
							  (interactive)
							  (start-of-paragraph-text)))
  (keymap-global-set "M-]" '(lambda ()
							  (interactive)
							  (end-of-paragraph-text)))
  (keymap-global-set "C-x M-c" '(lambda ()
								  (interactive)
								  (save-some-buffers)
								  (kill-emacs)))
  (keymap-global-set "C-x M-C" '(lambda ()
								  (interactive)
								  (save-some-buffers)
								  (restart-emacs))))
