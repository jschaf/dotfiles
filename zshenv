#!/bin/zsh

# Disable autocompletion setup by Google /etc/zshrc
# google_zsh_flysolo='I march to my own drum'

# General Settings
export LANG=en_US.UTF-8
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient -a emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"
export WORKON_HOME="$HOME/.dotfiles/home/.virtualenvs"
export PROJECT_HOME="$HOME/prog"
export DOTFILES_HOME="${HOME}/.dotfiles"
export TMUXP_CONFIGDIR="${HOME}/.dotfiles/tmuxp"
export ZDOTDIR="${HOME}/.zsh"

# Go setup
export GOPATH="${HOME}/prog"

# NodeJS and NPM setup.
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="${NPM_PACKAGES}/lib/node_modules:${NODE_PATH}"

# Ruby setup
export GEM_HOME="$HOME/.gems"

export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
# Path Setup
#
# ZSH ties the $path array variable to the $PATH environmental variable via
#`typeset -T`.  We can make the $path array only have unique entries with
#`typeset -U`

# automatically remove duplicates from these arrays
typeset -U path PATH cdpath CDPATH fpath FPATH manpath MANPATH
path=(
    ~/bin-system
    ~/bin
    ~/prog/bin
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
