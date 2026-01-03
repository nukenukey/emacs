;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025 aug <baleofhay@proton.me>

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons company counsel dashboard doom-modeline fireplace
				   fish-mode ivy-rich lsp-mode magit multiple-cursors
				   org-bullets php-mode pink-bliss-uwu-theme rust-mode
				   tldr typescript-mode vterm)))

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

(use-package dired
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
  :defer t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-agenda-files '("~/org/agenda")
        diary-file "~/.emacs.d/diary.gpg"
        org-html-validation-link nil
		org-agenda-start-with-log-mode t
		org-log-into-drawer t
		org-todo-keywords '((sequence "TODO(t)" "TOTURNIN(m)" "CURRENT(c)" "URGENT(u)" "DEFERRED(f)" "ASSIGNMENT(a)" "|" "DONE(d)" "NOTDOING(n)"))))

(use-package tramp
  :defer t
  :init
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not tramp-mode))))
  (keymap-global-set "C-x M-r" 'tramp-cleanup-all-buffers))

(use-package windmove
  :defer nil
  :config
  (keymap-global-set "M-H" 'windmove-left)
  (keymap-global-set "M-J" 'windmove-down)
  (keymap-global-set "M-K" 'windmove-up)
  (keymap-global-set "M-L" 'windmove-right))

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
  :defer t
  :config
  ;; (setq tab-bar-format nil)
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
  :init
  (setq recentf-exclude '("~/org/agenda/.*")
		recentf-max-saved-items 32)
  (add-to-list 'auto-save-hook #'recentf-save-list))

(use-package emacs
  :defer nil
  :config
  (setq inhibit-default-init t
		inhibit-startup-screen t
		ring-bell-function 'ignore
		tab-width 4)
  (custom-set-faces
   '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 145 :width normal :foundry "JB" :family "JetBrains Mono"))))
   '(cursor ((t (:background "sienna")))))
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

  ;; (dolist (file (file-expand-wildcards (concat user-emacs-directory "*.el")))
  ;; 	(load-file file))
  ;; (load-file (file-expand-wildcards (concat user-emacs-directory "*.el")))
  (load-file "~/.emacs.d/third-party.el")
  (load-file "~/.emacs.d/cornell.el")
  (load-file "~/.emacs.d/conv-org.el")
  (load-file "~/.emacs.d/conv-gpg.el")
  (load-file "~/.emacs.d/conv-gen.el")
  (load-file "~/.emacs.d/local.el")

  (setq initial-scratch-message nil)
  
  (setq scroll-conservatively 100)
  (setq shell-command-prompt-show-cwd t)
  (setq-default cursor-type 'bar)

  (scroll-bar-mode -1)
  (setq image-scaling-factor 1.0)

  (electric-pair-mode)
  (electric-indent-mode)
  (flyspell-mode)

  (keymap-global-set "C-x j r" 'counsel-recentf)
  (keymap-global-set "C-x j C-r" 'recentf-save-list)

  (keymap-global-set "C-x M-f" 'find-file-other-window)

  (setq-default tab-stop-list 4)
  (setq-default indent-tab-modes nil)
  (setq-default tab-always-indent nil)
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq-default c-basic-offset 4)
  (setq indent-line-function 'insert-tab)
  (setq split-width-threshold 1)
  (defvaralias 'c-basic-offset 'tab-width)

  (keymap-global-set "C-S-w" 'clipboard-kill-region)
  (keymap-global-set "M-W" 'clipboard-kill-ring-save)

  (keymap-global-set "C-=" 'text-scale-increase)
  (keymap-global-set "C--" 'text-scale-decrease)
  (keymap-global-set "C-s" 'swiper)
  (keymap-global-set "C-S-s" 'swiper-thing-at-point)
  (keymap-global-set "C-x f" 'query-replace)
  (keymap-global-set "C-x j c" 'conv/cornell-init)
  (keymap-global-set "C-x j a" 'conv/org-agenda-list)
  (keymap-global-set "C-x j d" 'conv/code-init)
  (keymap-global-set "C-x h" 'previous-buffer)
  (keymap-global-set "C-x l" 'next-buffer)
  (keymap-global-set "C-x j u" 'compile)

  (keymap-global-set "C-x C-r" 'tramp-revert-buffer-with-sudo)
  (unbind-key "C-x C-l")
  (keymap-global-set "C-x C-l l" 'count-lines-page)
  (keymap-global-set "C-x C-l p" 'check-parens)
  (keymap-set help-map "g" 'shortdoc-display-group)

  (keymap-global-set "C-k" 'kill-whole-line)
  (keymap-global-set "C-x C-a" 'mark-whole-buffer)
  (keymap-global-set "M-D" 'backward-kill-word)
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
  
  (keymap-global-set "s-x" 'counsel-linux-app)

  (keymap-global-set "M-/" 'hippie-expand)
  (keymap-global-set "C-x C-b" 'electric-buffer-list)
  (keymap-global-set "C-x b" 'counsel-switch-buffer)

  (keymap-global-set "C-x M-c" '(lambda ()
                                  (interactive)
                                  (save-some-buffers)
                                  (kill-emacs)))

  (keymap-global-set "C-x M-C" '(lambda ()
                                  (interactive)
                                  (save-some-buffers)
                                  (restart-emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 145 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(cursor ((t (:background "sienna")))))
