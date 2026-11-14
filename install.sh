#!/bin/sh
set -ex
for i in * ; do
		ln -s ~/.emacs.d/$i ./$i
done
