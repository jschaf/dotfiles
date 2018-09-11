#!/bin/bash

# This file is sourced from ~/.bash_profile and ~/.zsh/.zprofile

# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.

# Flags
export LOADED_SH_PROFILE="yes"

# General Settings
export TERM=xterm-24bit
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
export OSFONTDIR="$HOME/.local/share/fonts;$HOME/Library/Fonts;/usr/share/fonts"

export DIFF="colordiff -u "

if [[ -z $HOSTNAME ]]; then
  # For zsh compatibility with bash.
  export HOSTNAME=$HOST
fi

npm_auth_token_file="$HOME/.config/npm/npm-auth-token"
export NPM_AUTH_TOKEN="NOT_INITIALIZED_FROM_FILE"
if [[ -f "$npm_auth_token_file" ]]; then
  NPM_AUTH_TOKEN="$(< $HOME/.config/npm/npm-auth-token)"
fi
unset npm_auth_token_file

# Dotfiles
export DOTFILES_DIR=${HOME}/.dotfiles
export DOTFILES_WORK_DIR=${HOME}/.dotfiles-work

export PERSONAL_DICTIONARY=${HOME}/.config/personalDictionary/words.txt

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"

# Don't reload the ranger config since our custom config loads it all.
export RANGER_LOAD_DEFAULT_RC='FALSE'

# Go setup
DEFAULT_GOPATH="${HOME}/go"
DOTFILES_GOPATH="${DOTFILES_DIR}/go"
export GOPATH="${DEFAULT_GOPATH}:${DOTFILES_GOPATH}"

# NodeJS and NPM setup.
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"

# Ruby setup
export GEM_HOME="$HOME/.gems"

# Rust setup
export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

# Perforce
export P4CONFIG=.p4config
export P4DIFF="colordiff -u "
export P4MERGE=/google/src/files/head/depot/eng/perforce/mergep4.tcl
export P4EDITOR=$EDITOR

OLD_PATH="$PATH"
export PATH="$HOME/bin"
PATH+=":$HOME/.dotfiles-work/host-${HOSTNAME}/bin"
PATH+=":$HOME/.dotfiles-work/bin"
PATH+=":$HOME/.dotfiles/bin"
PATH+=":$HOME/.dotfiles/zsh/iosource"
PATH+=":${DEFAULT_GOPATH}/bin"
PATH+=":${DOTFILES_GOPATH}/bin"
PATH+=":$HOME/prog/flutter/bin"
PATH+=":/usr/local/homebrew/bin"
PATH+=":$HOME/.cask/bin"
PATH+=":$HOME/.cargo/bin"
PATH+=":$HOME/.yarn/bin"
# Setup Ruby and Gem so we install packages without root.
PATH+=":${GEM_HOME}/bin"
# Setup NPM so we can install global packages without root.  See
# http://stackoverflow.com/questions/10081293.
PATH+=":${NPM_PACKAGES}/bin"
PATH+=":/usr/local/bin"
PATH+=":/usr/share/texmf-dist/scripts/texlive"
PATH+=":$OLD_PATH"

OLD_MANPATH="$MANPATH"
export MANPATH=":${NPM_PACKAGES}/share/man"
MANPATH+=":/usr/man"
MANPATH+=":/usr/local/man"
MANPATH+=":$OLD_MANPATH"
