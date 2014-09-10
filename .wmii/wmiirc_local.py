import wmiirc
from subprocess import call
import pygmi

pygmi.wmii['font'] = 'xft:Consolas-11'
# text color, background, border
pygmi.wmii['normcolors'] = '#cecece', '#1f1f1f', '#1f1f1f'
pygmi.wmii['focuscolors'] = '#cecece', '#1f1f1f', '#6ca6cd'
pygmi.wmii['border'] = 1

wmiirc.terminal = 'wmiir', 'setsid', 'urxvt'



