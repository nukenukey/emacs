#!/usr/bin/env fish
argparse -x c,F -x i,c -x i,F -x i,f -x i,e -x i,s i/install c/client F/fullscreen f/floating e/eval= s/select_frame -- $argv
or return

if set -q _flag_i
		if [ command -q stow ]
				stow -t ~/.emacs.d/ .
		else
				echo "stow must be installed" >&2
				exit 1
		end
end

if set -q _flag_c
		set emacs_command "emacsclient -c -a ''"
else
		set emacs_command "emacs"
end

if set -q _flag_F
		set -a emacs_command "-fs"
end

if set -q _flag_f
		set -a emacs_command "-F '((name . \"floating\"))'" # this requires some work on your window manager's side
end

if set -q _flag_e
		set -a emacs_command "-e $_flag_e"
end

echo "$emacs_command"
eval "$emacs_command"
