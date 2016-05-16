# Path to Oh My Fish install.
set -gx OMF_PATH "/home/joe/.local/share/omf"

set fish_greeting ""

set -gx PATH "/home/joe/.cask/bin" $PATH
set -gx PATH "/home/joe/.local/bin" $PATH
set -x GPG_TTY (tty)
# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/home/joe/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish
