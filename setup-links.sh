#!/bin/sh

DIR="$( cd "$( dirname "$0" )" && pwd )"

DOTFILES=".aliases
.bashrc
.bash_profile
.gitconfig
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
.zshrc.functions
.zprofile"

for file in $DOTFILES
do
    if [ ! -f "$HOME/${file}" ]
    then
        ln -s "${DIR}/${file}" "$HOME"
    fi
done


DOTFOLDERS=".xmonad
.mutt "

for folder in $DOTFOLDERS
do
    if [ ! -d "$HOME/${folder}" ]
    then
        ln -s "${DIR}/${folder}" "$HOME"
    fi
done