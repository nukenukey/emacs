#!/bin/sh
emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
