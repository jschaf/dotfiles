#!/bin/sh

# Add copy-paste support for virtualbox
vboxProgram=/usr/bin/VBoxClient-all
if [ -f $vboxProgram ]; then
    $vboxProgram &
fi

userresources=$HOME/.Xresources

# Load .Xresources
if [ -f $userresources ]; then
    xrdb -merge "$userresources" &
fi

# Set a background
backgroundpic=$HOME/.background.jpg
if [ -f $backgroundpic ]; then
    feh --bg-center "$backgroundpic" &
fi

# Keyboard config
xmodmaprc=$HOME/.xmodmaprc
if [ -f $xmodmaprc ]; then
    xmodmap "$xmodmaprc"
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

