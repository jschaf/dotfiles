
# Start X at boot
# 
# https://wiki.archlinux.org/index.php/Start_X_at_Boot
if [[ -z $DISPLAY && $(tty) = /dev/tty1 ]]; then
  exec startx
  # Could use xinit instead of startx
  #exec xinit -- /usr/bin/X -nolisten tcp
fi

