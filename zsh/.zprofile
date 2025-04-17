#!/bin/zsh

export _SOURCED_ZSH_ZPROFILE='yes'

# General config
export XDG_CONFIG_HOME="${HOME}/.config"

# Locale
export LANG=c
export LC_ALL=C
export LC_MESSAGES=C

# Editors
export PAGER=less
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -nw"
export VISUAL="emacsclient --alternate-editor=emacs"
export DOOMDIR="${DOTFILES_HOME}/doom"

# OS
export OS_TYPE
OS_TYPE="$(uname -s)"
function is-linux() { [[ "${OS_TYPE}" == "Linux" ]]; }
function is-darwin() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-macos() { [[ "${OS_TYPE}" == "Darwin" ]]; }
function is-freebsd() { [[ "${OS_TYPE}" == "FreeBSD" ]]; }

# Returns 0 if the current terminal is a TTY.
#
# TTY is ambiguous, but I'm using it to mean where at a framebuffer terminal
# that doesn't have UTF-8 and is limited to 8 colors.
function is-tty() {
  is-linux && [[ $(tty) == /dev/tty[0-9] ]]
}
# Force PATH to be unique; the path array is tied to $PATH (typeset -t).
# shellcheck disable=SC2034
typeset -U path

if is-macos; then
  export JAVA_HOME=/Library/Java/JavaVirtualMachines/temurin-11.jdk/Contents/Home
  path+=(
    /usr/local/bin
    /System/Cryptexes/App/usr/bin
    /usr/bin
    /bin
    /usr/sbin
    /sbin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin
    /var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin
    /Library/Apple/usr/bin
    /opt/homebrew/bin
    "$HOME/bin"
    "$HOME/.orbstack/bin"
    "$HOME/.local/bin"
    "$HOME/go/bin"
    "$HOME/Library/Application Support/JetBrains/Toolbox/scripts"
    "/opt/homebrew/opt/postgresql@15/bin"
  )
  fpath+=/Applications/OrbStack.app/Contents/Resources/completions/zsh
  export MANPATH="/opt/homebrew/share/man:$MANPATH"
fi

# Print more info for time command.
# https://unix.stackexchange.com/a/562651/179300
#
# wall-time:   2700ms
# user-time:   243ms
# kernel-time: 2260ms
# cpu-percent: 92%  (243ms + 2260ms)/2700ms
# max-memory:  36320kB
# page-faults: 291 major, 84820 minor
# input-ops:   0
# output-ops:  0
# ctx-switch:  3675 voluntary, 22249 involuntary
# shellcheck disable=SC2034
TIMEFMT=$'\n wall-time:   %mE \n user-time:   %mU\n kernel-time: %mS \n cpu-percent: %P  (%mU + %mS)/%mE \n max-memory:  %MkB \n page-faults: %F major, %R minor \n input-ops:   %I \n output-ops:  %O \n ctx-switch:  %w voluntary, %c involuntary\n'

# Hide homebrew hints
export HOMEBREW_NO_ENV_HINTS=1
