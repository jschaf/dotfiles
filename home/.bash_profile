if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

