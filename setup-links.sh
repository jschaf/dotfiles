#!/bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

DOTFILES=".bashrc
.xinitrc
.Xresources
.xmonad
.xmobarrc"

for file in $DOTFILES
do
    if [ ! -f "$HOME/${file}" ]
    then
        ln -s "${DIR}/${file}" "$HOME"
    fi
done

