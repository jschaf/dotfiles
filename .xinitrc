#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Key bindings
if [ -f $HOME/.xmodmaprc ]; then
    /usr/bin/xmodmap $HOME/.xmodmaprc
fi

# Add copy-paste support for virtualbox
VBoxClient-all &

userresources=$HOME/.Xresources

# Load .Xresources
if [ -f $userresources ]; then
    xrdb -merge "$userresources" &
fi


# Keyboard config
xmodmaprc=$HOME/.xmodmaprc
if [ -f $xmodmaprc ]; then
    xmodmap "$xmodmaprc"
fi

# Start a terminal daemon
/usr/bin/urxvtd --quiet --opendisplay --fork

xbindkeys 2> /dev/null &

# Set a background
feh --bg-fill ~/.desktop.jpg

# Startup programs
urxvtc -e bash -c "tmux attach"  &
emacs &

# Enable zapping
setxkbmap -option terminate:ctrl_alt_bksp

# Hide the cursor until it's moved
unclutter -idle 3 -root -jitter 10 &

# Start the window manager
exec wmii -r /etc/wmii/python/wmiirc
