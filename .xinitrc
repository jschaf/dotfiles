#!/bin/sh

userresources=$HOME/.Xresources

# Load up my .Xresources
if [ -f $userresources ]; then
    xrdb -merge "$userresources"
fi

/usr/bin/VBoxClient-all

# Enable zapping
setxkbmap -option terminate:ctrl_alt_bksp

# Start the window manager
xmonad
