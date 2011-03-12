#!/bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

DOTFILES=".aliases
.bashrc
.ghci
.msmtprc
.offlineimaprc
.tmux.conf
.xinitrc
.xmobarrc
.xmodmaprc
.Xresources
.zshrc
.zshrc.aliases
.zshrc.functions"

for file in $DOTFILES
do
    if [ ! -f "$HOME/${file}" ]
    then
        ln -s "${DIR}/${file}" "$HOME"
    fi
done


DOTFOLDERS=".xmonad
.mutt
.newsbeuter"

for folder in $DOTFOLDERS
do
    if [ ! -d "$HOME/${folder}" ]
    then
        ln -s "${DIR}/${folder}" "$HOME"
    fi
done