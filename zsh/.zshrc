#!/bin/zsh


# ZSH loads the following files in order. $ZDOTDIR is used instead of $HOME if
# set.
# 1. /etc/zsh/zshenv (always)
# If NO_RCS is set, none of the following are run.
# 2. ~/.zshenv (Usually run for all shells)
# 3. /etc/zsh/zprofile (login)
# 4. ~/.zprofile (login)
# 5. /etc/zsh/zshrc (interactive)
# 6 ~/.zshrc (interactive)
# 7. /etc/zsh/zlogin (login)
# 8. ~/.zlogin (login)
#
# If a login shell, the following are run on logout or exit.
# ~/.zlogout
# /etc/zlogout

# History
HISTFILE=${ZDOTDIR:-${HOME}}/.zsh_history
# The maximum number of history events to save in the history file.
export SAVEHIST=50000
# The maximum number of events stored in the internal history list.
export HISTSIZE=10000

# Append history list to the history file; this is the default but we make sure
# because it's required for share_history.
setopt append_history

setopt inc_append_history

# Make # work at beginning of commands.
setopt interactive_comments

# Import new commands from the history file also in other zsh-session.
setopt share_history

# Save each command's beginning timestamp and the duration to the history file.
setopt extended_history

# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list.
setopt hist_ignore_all_dups

# Remove command lines from the history list when the first character on the
# line is a space.
setopt hist_ignore_space

# If a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
setopt auto_cd

# In order to use #, ~ and ^ for filename generation grep word
# *~(*.gz|*.bz|*.bz2|*.zip|*.Z) -> searches for word not in compressed files
# don't forget to quote '^', '~' and '#'!.
setopt extended_glob

# Display PID when suspending processes as well.
setopt long_list_jobs

# Report the status of backgrounds jobs immediately.
setopt notify

# Whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all

# Not just at the end.
setopt complete_in_word

# Don't send SIGHUP to background processes when the shell exits.
setopt nohup

# Make cd push the old directory onto the directory stack.
setopt auto_pushd

# Avoid "beep"ing.
setopt nobeep

# Don't push the same dir twice.
setopt pushd_ignore_dups

# * shouldn't match dotfiles. ever.
setopt no_glob_dots

# Use zsh style word splitting.
setopt no_sh_word_split

# Don't error out when unset parameters are used.
setopt unset

# Load a few modules.
for mod in parameter complist deltochar mathfunc ; do
  zmodload -i zsh/${mod} 2>/dev/null || print "Notice: no ${mod} available :("
done && builtin unset -v mod

autoload zmv
autoload zed

typeset -ga ls_options
ls_options+=( --color=auto )

if is-macos; then
  export CLICOLOR=1 # https://unix.stackexchange.com/a/2904
fi

# Setup colors setup for ls.
if is-linux; then
  eval "$(dircolors -b)"
fi

# Support colors in less.  This is only needed for interactive shells, so
# keep it here instead of in zshenv.
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# (F) Quits if only one screen.
# (i) Ignore case in searches.
# (J) Displays a status column at left showing matching lines.
# (M) Long prompt.
# (R) Allow ansi color escapes.
# (w) Highlight unread lines on forward movement.
# (X) Disable sending termcap init.
export LESS="-FiJMRwX"

# ESC-h Call run-help for the 1st word on the command line
autoload -Uz run-help
autoload -Uz run-help-git
autoload -Uz run-help-openssl
autoload -Uz run-help-sudo

# Hide homebrew hints.
export HOMEBREW_NO_ENV_HINTS=1

# Colors
autoload colors
# shellcheck disable=SC2154
if [[ $terminfo[colors] -gt 8 ]]; then
  colors
fi

if [[ $TERM != 'dumb' ]]; then
  source "${ZSH_DOTFILES}/plugins.zsh"
  source "${ZSH_DOTFILES}/completions.zsh"
  source "${ZSH_DOTFILES}/keys.zsh"
fi
source "${ZSH_DOTFILES}/aliases.zsh"

[[ -f "$ZDOTDIR/private.zsh" ]] && source "$ZDOTDIR/private.zsh"
