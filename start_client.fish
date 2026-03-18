#!/usr/bin/env fish
argparse c/client F/fullscreen f/floating e/eval= s/select_frame -- $argv

#TODO: make select frame / auto maximize functionality

if set -q _flag_c
		set emacs_command "emacsclient -c -a '' "
else
		set emacs_command "emacs "
end

if set -q _flag_F
		if set -q _flag_c
				echo "can't do fullscreen emacsclient, boss"
		else
				set -a emacs_command "-fs "
		end
end

if set -q _flag_f
		set -a emacs_command "-F '(name . \"floating\")' " # this requires some work on your window manager's side
end

if set -q _flag_e
		set to_eval
end

eval "$emacs_command"
