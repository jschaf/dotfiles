#!/bin/sh
# ~/.profile is read by the display manager.

export _SOURCED_PROFILE='yes'

# General config
export DOTFILES_HOME="/p/dotfiles"
export DOTFILES_WORK="/p/dotfiles-work"
export XDG_CONFIG_HOME="${HOME}/.config"

export OS_TYPE
OS_TYPE="$(uname -s)"

export DISTRO_TYPE='unknown'
if [ -r /etc/arch-release ]; then DISTRO_TYPE='arch'; fi
if [ -r /etc/debian_version ]; then DISTRO_TYPE='debian'; fi

# Set terminal property (used by msgid-chooser).
export COLORTERM="yes"
export CLICOLOR=1
export PAGER=${PAGER:-less}

# Editors
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export FPP_EDITOR="emacsclient --no-wait -a emacs"

# Go setup
export GOPATH='/go'

# Ruby setup
export GEM_HOME="$HOME/.gems"

# Rust setup
export RUST_SRC_PATH="${HOME}/.multirust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

# For zsh compatibility with bash.
if [ -z "$HOSTNAME" ]; then
  export HOSTNAME=$HOST
fi

# prepend_to_path adds args the beginning of the path
# https://unix.stackexchange.com/a/4973/179300
prepend_to_path() {
  for d; do
    if [ -z "$d" ]; then continue; fi # skip nonexistent directory
    case ":$PATH:" in
    # skip if path already in entry
    *":$d:"*) : ;;
    *) PATH=$d:$PATH ;;
    esac
  done
}

# Prepend in reverse order, so the last entry has the highest priority.
prepend_to_path \
  "/usr/share/texmf-dist/scripts/texlive" \
  "/usr/local/bin" \
  "${GEM_HOME}/bin" \
  "${HOME}/.yarn/bin" \
  "${HOME}/.cargo/bin" \
  "${HOME}/.cask/bin" \
  "${GOPATH}/bin" \
  "${DOTFILES_HOME}/zsh/iosource" \
  "${HOME}/bin" \
  "${DOTFILES_HOME}/bin" \
  "${DOTFILES_WORK}/bin"
