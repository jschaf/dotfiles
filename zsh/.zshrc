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

# Directories that we can cd to without prefixing the path.
# shellcheck disable=SC2034
cdpath=(/opt/p)

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
typeset -ga grep_options
if ls --color=auto / >/dev/null 2>&1; then
  ls_options+=( --color=auto )
elif ls -G / >/dev/null 2>&1; then
  ls_options+=( -G )
fi
grep_options+=( --color=auto )

for var in LANG LC_ALL LC_MESSAGES ; do
  [[ -n ${(P)var} ]] && export $var
done && unset -v var

# Setup colors setup for ls.
external-command-exists dircolors && eval $(dircolors -b)

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
alias run-help >&/dev/null && unalias run-help
for rh in run-help{,-git,-ip,-openssl,-p4,-sudo,-svk,-svn}; do
  autoload $rh
done; unset rh

# wonderful idea of using "e" glob qualifier by Peter Stephenson
# You use it as follows:
# $ NTREF=/reference/file
# $ ls -l *(e:nt:)
# This lists all the files in the current directory newer than the reference file.
# You can also specify the reference file inline; note quotes:
# $ ls -l *(e:'nt ~/.zshenv':)
function nt () {
  if [[ -n $1 ]] ; then
    local NTREF=${~1}
  fi
  [[ $REPLY -nt $NTREF ]]
}

# Provides useful information on globbing
function H-Glob () {
  echo -e "
    /      directories
    .      plain files
    @      symbolic links
    =      sockets
    p      named pipes (FIFOs)
    *      executable plain files (0100)
    %      device files (character or block special)
    %b     block special files
    %c     character special files
    r      owner-readable files (0400)
    w      owner-writable files (0200)
    x      owner-executable files (0100)
    A      group-readable files (0040)
    I      group-writable files (0020)
    E      group-executable files (0010)
    R      world-readable files (0004)
    W      world-writable files (0002)
    X      world-executable files (0001)
    s      setuid files (04000)
    S      setgid files (02000)
    t      files with the sticky bit (01000)

  print *(m-1)          # Files modified up to a day ago
  print *(a1)           # Files accessed a day ago
  print *(@)            # Just symlinks
  print *(Lk+50)        # Files bigger than 50 kilobytes
  print *(Lk-50)        # Files smaller than 50 kilobytes
  print **/*.c          # All *.c files recursively starting in \$PWD
  print **/*.c~file.c   # Same as above, but excluding 'file.c'
  print (foo|bar).*     # Files starting with 'foo' or 'bar'
  print *~*.*           # All Files that do not contain a dot
  chmod 644 *(.^x)      # make all plain non-executable files publically readable
  print -l *(.c|.h)     # Lists *.c and *.h
  print **/*(g:users:)  # Recursively match all files that are owned by group 'users'
  echo /proc/*/cwd(:h:t:s/self//) # Analogous to >ps ax | awk '{print $1}'<"
}
alias help-zshglob=H-Glob


# Colors
autoload colors
if [[ "$terminfo[colors]" -gt 8 ]]; then
  colors
fi

source "${ZSH_DOTFILES}/plugins.zsh"
source "${ZSH_DOTFILES}/completions.zsh"
source "${ZSH_DOTFILES}/keys.zsh"
source "${ZSH_DOTFILES}/aliases.zsh"
is-debian-distro && source "${ZSH_DOTFILES}/debian.zsh"
is-macos && source "${ZSH_DOTFILES}/macos.zsh"

source-if-exists "${ZSH_WORK_DOTFILES}/work.zsh"
source-if-exists "${ZSH_WORK_DOTFILES}/host.zsh"

fpath+=("$ZSH_WORK_DOTFILES")

