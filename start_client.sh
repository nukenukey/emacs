#!/bin/sh
emacsclient -c -a '' -e "(progn (toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame)) (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer)) (dired-jump)))"
