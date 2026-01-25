;; -*- lexical-binding: t; -*-

;; Copyright (C) 2025 aug <baleofhay@proton.me>

;; just go fucking crazy
(setq gc-cons-threshold (* 512 1024 1024) ;; you can go up to 512mb before garbage collecting, most-positive-fixnum didn't seem to improve the init time
      gc-cons-percentage 0.8) ;; you can go up to 80 percent of the heap before garbage collection

;; do some optimize :>
(setq native-comp-async-jobs-number 0) ;; use half of the cpu cores for native compilation - this value is supposed to be 0, not a mistake

;; initial frame options
(set-fringe-mode 0)
(scroll-bar-mode -1)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)

(setq use-short-answers t ;; just y and n pls
      confirm-kill-emacs 'yes-or-no-p
	  inhibit-default-init t ;; default init stuff
	  inhibit-startup-screen t
	  ring-bell-function 'ignore
	  word-wrap	nil
	  mouse-autoselect-window t)

(add-to-list 'default-frame-alist '(height . 25))
(add-to-list 'default-frame-alist '(width . 75))

;; alright now relax a little
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold (* 80 100 100)
                                   gc-cons-percentage 0.1))) ;; when emacs is settled down, make the thresholds more reasonable
