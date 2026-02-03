#!/bin/sh

if [ "$XDG_CURRENT_DESKTOP" != "sway" ] ; then
	SELECT_FRAME="(toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame))"
else
	SELECT_FRAME=""
fi

if [ -d "/usr/share/cowsay/cows" -a $(command -v cowsay) -a $(command -v cowsay) ] ; then
	COWSAY_CMD="fortune -c | cowsay -nf $(ls /usr/share/cowsay/cows | shuf | head -1)"
	FORTUNE_COWSAY_BUFFER_OR_DIRED="(with-current-buffer (get-buffer-create \"*fortune*\") (insert (shell-command-to-string \"${COWSAY_CMD}\"))) (switch-to-buffer \"*fortune*\")))"
else
	FORTUNE_COWSAY_BUFFER_OR_DIRED="(dired-jump)"
fi

emacsclient -c -a '' -e "(progn ${SELECT_FRAME} (or (and (boundp 'conv/last-buffer) (conv/switch-to-last-buffer))) ${FORTUNE_COWSAY_BUFFER_OR_DIRED}"
