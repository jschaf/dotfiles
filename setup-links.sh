#!/bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

DOTFILES=".xinitrc
.Xresources
.xmonad"

for file in $DOTFILES
do
    ln -s "${DIR}/${file}" "$HOME"
done

