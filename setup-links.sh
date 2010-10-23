#!/bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

DOTFILES=".bashrc
.msmtprc
.offlineimaprc
.tmux.conf
.xinitrc
.xmobarrc
.Xresources
.zshrc"

for file in $DOTFILES
do
    if [ ! -f "$HOME/${file}" ]
    then
        ln -s "${DIR}/${file}" "$HOME"
    fi
done


DOTFOLDERS=".xmonad
.mutt"

for folder in $DOTFOLDERS
do
    if [ ! -d "$HOME/${folder}" ]
    then
        ln -s "${DIR}/${folder}" "$HOME"
    fi
done