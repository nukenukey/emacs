#!/usr/bin/env bash
if [ "$XDG_CURRENT_DESKTOP" != "sway" ] ; then
	SELECT_FRAME="(toggle-frame-fullscreen) (select-frame-set-input-focus (selected-frame))"
else
	SELECT_FRAME=""
fi

ACTION="(dired-jump)"

emacsclient -a '' -c -e "(progn ${SELECT_FRAME} ${INITIAL_ACTION})" || { notify-send "emacsclient could not be started" ; return 1 ; }
