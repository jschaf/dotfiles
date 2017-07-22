#!/bin/zsh

is-profiling-zsh && zsup-beginning-of-startup-file

# General Settings
export TERM="xterm-256color"
export WORKON_HOME="$HOME/.dotfiles/home/.virtualenvs"
export PROJECT_HOME="$HOME/prog"
export DOTFILES_HOME="${HOME}/.dotfiles"
export TMUXP_CONFIGDIR="${HOME}/.dotfiles/tmuxp"
export WALLPAPER_HOME="${HOME}/.config/wallpapers"
export XDG_CONFIG_HOME="${HOME}/.config"
export GRML_OSTYPE="$(uname -s)"
# Set terminal property (used e.g. by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}
export OSFONTDIR="$HOME/.local/share/fonts;$HOME/Library/Fonts"

npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
export NPM_AUTH_TOKEN="NOT_INITIALIZED_FROM_FILE"
if [[ -f "$npm_auth_token_file" ]]; then
  NPM_AUTH_TOKEN="$(< $HOME/.config/npm/npm-auth-token)"
fi
unset npm_auth_token_file

# Dotfiles
export DOTFILES_DIR=${HOME}/.dotfiles
export DOTFILES_WORK_DIR=${HOME}/.dotfiles-work
export DOTFILES_VENDOR_DIR=${DOTFILES_DIR}/vendor

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"

# Go setup
export GOPATH="${HOME}/prog"

# NodeJS and NPM setup.
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"

# Ruby setup
export GEM_HOME="$HOME/.gems"

# Rust setup
export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

# Path Setup
#
# ZSH ties the $path array variable to the $PATH environmental variable via
#`typeset -T`.  We can make the $path array only have unique entries with
#`typeset -U`
typeset -U path PATH cdpath CDPATH fpath FPATH manpath MANPATH
path=(
  ~/bin-system
  ~/bin
  ~/.dotfiles-work/bin
  ~/.dotfiles/bin
  ~/prog/bin
  ~/prog/flutter/bin
  ~/homebrew/opt/coreutils/libexec/gnubin
  ~/homebrew/bin
  ~/.cask/bin
  ~/.cargo/bin
  ~/.yarn/bin
  # Setup Ruby and Gem so we install packages without root.
  ${GEM_HOME}/bin
  # Setup NPM so we can install global packages without root.  See
  # http://stackoverflow.com/questions/10081293.
  ${NPM_PACKAGES}/bin
  /usr/local/bin
  /usr/share/texmf-dist/scripts/texlive
  $path
)

manpath=(
  ${NPM_PACKAGES}/share/man
  /usr/man
  /usr/local/man
  $manpath
)

# If we have don't have a display and we're on TTY1.
if [[ -z "$DISPLAY" ]] && [[ -n "$XDG_VTNR" ]] && [[ "$XDG_VTNR" -eq 1 ]]; then
  exec startx ~/.config/X11/xinitrc
fi

is-profiling-zsh && zsup-end-of-startup-file
