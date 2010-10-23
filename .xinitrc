#!/bin/sh

# Add copy-paste support
/usr/bin/VBoxClient-all &

userresources=$HOME/.Xresources

# Load up my .Xresources
if [ -f $userresources ]; then
    xrdb -merge "$userresources"
fi

backgroundpic=$HOME/.background.jpg
if [ -f $backgroundpic ]; then
    xsetbg "$backgroundpic" &
fi

# Startup programs
chromium &
urxvt  &
emacs &

# Enable zapping
setxkbmap -option terminate:ctrl_alt_bksp

# XMonad doesn't set a default cursor, so it defaults to the ugly
# cross.  This sets it as the normal left pointer.
xsetroot -cursor_name left_ptr

# Hide the cursor until it's moved
unclutter -idle 2 -root -jitter 10 &


# Start the window manager
xmonad
 
