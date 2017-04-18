#!/bin/zsh

# ZSH loads the following files in order.
# 1. /etc/zshenv (always)
# If NO_RCS is set, none of the following are run.
# 2. ~/.zshenv (Usually run for all shells)
# 3. /etc/zprofile (login)
# 4. ~/.zprofile (login)
# 5. /etc/zshrc (interactive)
# 6 ~/.zshrc (interactive)
# 7. /etc/zlogin (login)
# 8. ~/.zlogin (login)
#
# If a login shell, the following are run on logout or exit.
# ~/.zlogout
# /etc/zlogout

# remove-nonexistent-paths manpath
# remove-nonexistent-paths path

GRML_OSTYPE=$(uname -s)

function islinux () {
    [[ $GRML_OSTYPE == "Linux" ]]
}

function isdarwin () {
    [[ $GRML_OSTYPE == "Darwin" ]]
}

function isfreebsd () {
    [[ $GRML_OSTYPE == "FreeBSD" ]]
}

function isopenbsd () {
    [[ $GRML_OSTYPE == "OpenBSD" ]]
}

function issolaris () {
    [[ $GRML_OSTYPE == "SunOS" ]]
}

function source-if-exists() {
    [[ -e "$1" ]] && source "$1"
}

# check for user, if not running as root set $SUDO to sudo
(( EUID != 0 )) && SUDO='sudo' || SUDO=''


# set some important options (as early as possible)

# History
HISTFILE=${ZDOTDIR:-${HOME}}/.zsh_history
export SAVEHIST=10000
export HISTSIZE=1000

# append history list to the history file; this is the default but we make sure
# because it's required for share_history.
setopt append_history

# Make # work at beginning of commands.
setopt interactive_comments

# import new commands from the history file also in other zsh-session
setopt share_history

# save each command's beginning timestamp and the duration to the history file
setopt extended_history

# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
setopt hist_ignore_all_dups

# remove command lines from the history list when the first character on the
# line is a space
setopt hist_ignore_space

# if a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
setopt auto_cd

# in order to use #, ~ and ^ for filename generation grep word
# *~(*.gz|*.bz|*.bz2|*.zip|*.Z) -> searches for word not in compressed files
# don't forget to quote '^', '~' and '#'!
setopt extended_glob

# display PID when suspending processes as well
setopt longlistjobs

# report the status of backgrounds jobs immediately
setopt notify

# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all

# not just at the end
setopt completeinword

# Don't send SIGHUP to background processes when the shell exits.
setopt nohup

# make cd push the old directory onto the directory stack.
setopt auto_pushd

# avoid "beep"ing
setopt nobeep

# don't push the same dir twice.
setopt pushd_ignore_dups

# * shouldn't match dotfiles. ever.
setopt noglobdots

# use zsh style word splitting
setopt noshwordsplit

# don't error out when unset parameters are used
setopt unset

# setting some default values
NOCOR=${NOCOR:-0}
COMMAND_NOT_FOUND=${COMMAND_NOT_FOUND:-0}
GRML_ZSH_CNF_HANDLER=${GRML_ZSH_CNF_HANDLER:-/usr/share/command-not-found/command-not-found}

typeset -ga ls_options
typeset -ga grep_options
if ls --color=auto / >/dev/null 2>&1; then
    ls_options+=( --color=auto )
elif ls -G / >/dev/null 2>&1; then
    ls_options+=( -G )
fi
if grep --color=auto -q "a" <<< "a" >/dev/null 2>&1; then
    grep_options+=( --color=auto )
fi

# utility functions
# this function checks if a command exists and returns either true
# or false. This avoids using 'which' and 'whence', which will
# avoid problems with aliases for which on certain weird systems. :-)
# Usage: check_com [-c|-g] word
#   -c  only checks for external commands
#   -g  does the usual tests and also checks for global aliases
function check_com () {
    emulate -L zsh
    local -i comonly gatoo
    comonly=0
    gatoo=0

    if [[ $1 == '-c' ]] ; then
        comonly=1
        shift 1
    elif [[ $1 == '-g' ]] ; then
        gatoo=1
        shift 1
    fi

    if (( ${#argv} != 1 )) ; then
        printf 'usage: check_com [-c|-g] <command>\n' >&2
        return 1
    fi

    if (( comonly > 0 )) ; then
        (( ${+commands[$1]}  )) && return 0
        return 1
    fi

    if     (( ${+commands[$1]}    )) \
        || (( ${+functions[$1]}   )) \
        || (( ${+aliases[$1]}     )) \
        || (( ${+reswords[(r)$1]} )) ; then
        return 0
    fi

    if (( gatoo > 0 )) && (( ${+galiases[$1]} )) ; then
        return 0
    fi

    return 1
}

# creates an alias and precedes the command with
# sudo if $EUID is not zero.
function salias () {
    emulate -L zsh
    local only=0 ; local multi=0
    local key val
    while getopts ":hao" opt; do
        case $opt in
            o) only=1 ;;
            a) multi=1 ;;
            h)
                printf 'usage: salias [-hoa] <alias-expression>\n'
                printf '  -h      shows this help text.\n'
                printf '  -a      replace '\'' ; '\'' sequences with '\'' ; sudo '\''.\n'
                printf '          be careful using this option.\n'
                printf '  -o      only sets an alias if a preceding sudo would be needed.\n'
                return 0
                ;;
            *) salias -h >&2; return 1 ;;
        esac
    done
    shift "$((OPTIND-1))"

    if (( ${#argv} > 1 )) ; then
        printf 'Too many arguments %s\n' "${#argv}"
        return 1
    fi

    key="${1%%\=*}" ;  val="${1#*\=}"
    if (( EUID == 0 )) && (( only == 0 )); then
        alias -- "${key}=${val}"
    elif (( EUID > 0 )) ; then
        (( multi > 0 )) && val="${val// ; / ; sudo }"
        alias -- "${key}=sudo ${val}"
    fi

    return 0
}

for var in LANG LC_ALL LC_MESSAGES ; do
    [[ -n ${(P)var} ]] && export $var
done
builtin unset -v var


export PAGER=${PAGER:-less}

# color setup for ls:
check_com -c dircolors && eval $(dircolors -b)
# color setup for ls on OS X / FreeBSD:
isdarwin && export CLICOLOR=1
isfreebsd && export CLICOLOR=1

# Support colors in less.
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Load a few modules
for mod in parameter complist deltochar mathfunc ; do
    zmodload -i zsh/${mod} 2>/dev/null || print "Notice: no ${mod} available :("
done && builtin unset -v mod


# autoloading

autoload zmv
autoload zed


# ESC-h Call run-help for the 1st word on the command line
alias run-help >&/dev/null && unalias run-help
for rh in run-help{,-git,-ip,-openssl,-p4,-sudo,-svk,-svn}; do
    autoload $rh
done; unset rh

# command not found handling

(( ${COMMAND_NOT_FOUND} == 1 )) &&
function command_not_found_handler () {
    emulate -L zsh
    if [[ -x ${GRML_ZSH_CNF_HANDLER} ]] ; then
        ${GRML_ZSH_CNF_HANDLER} $1
    fi
    return 1
}

# dirstack handling

DIRSTACKSIZE=${DIRSTACKSIZE:-20}
DIRSTACKFILE=${DIRSTACKFILE:-${ZDOTDIR:-${HOME}}/.zdirs}

if zstyle -T ':grml:chpwd:dirstack' enable; then
    typeset -gaU GRML_PERSISTENT_DIRSTACK
    function grml_dirstack_filter () {
        local -a exclude
        local filter entry
        if zstyle -s ':grml:chpwd:dirstack' filter filter; then
            $filter $1 && return 0
        fi
        if zstyle -a ':grml:chpwd:dirstack' exclude exclude; then
            for entry in "${exclude[@]}"; do
                [[ $1 == ${~entry} ]] && return 0
            done
        fi
        return 1
    }

    function chpwd () {
        (( ZSH_SUBSHELL )) && return
        (( $DIRSTACKSIZE <= 0 )) && return
        [[ -z $DIRSTACKFILE ]] && return
        grml_dirstack_filter $PWD && return
        GRML_PERSISTENT_DIRSTACK=(
            $PWD "${(@)GRML_PERSISTENT_DIRSTACK[1,$DIRSTACKSIZE]}"
        )
        builtin print -l ${GRML_PERSISTENT_DIRSTACK} >! ${DIRSTACKFILE}
    }

    if [[ -f ${DIRSTACKFILE} ]]; then
        # Enabling NULL_GLOB via (N) weeds out any non-existing
        # directories from the saved dir-stack file.
        dirstack=( ${(f)"$(< $DIRSTACKFILE)"}(N) )
        # "cd -" won't work after login by just setting $OLDPWD, so
        [[ -d $dirstack[1] ]] && cd -q $dirstack[1] && cd -q $OLDPWD
    fi

    if zstyle -t ':grml:chpwd:dirstack' filter-on-load; then
        for i in "${dirstack[@]}"; do
            if ! grml_dirstack_filter "$i"; then
                GRML_PERSISTENT_DIRSTACK=(
                    "${GRML_PERSISTENT_DIRSTACK[@]}"
                    $i
                )
            fi
        done
    else
        GRML_PERSISTENT_DIRSTACK=( "${dirstack[@]}" )
    fi
fi

# directory based profiles

# chpwd_profiles(): Directory Profiles, Quickstart:
#
# In .zshrc.local:
#
#   zstyle ':chpwd:profiles:/usr/src/grml(|/|/*)'   profile grml
#   zstyle ':chpwd:profiles:/usr/src/debian(|/|/*)' profile debian
#   chpwd_profiles
#
# For details see the `grmlzshrc.5' manual page.
function chpwd_profiles () {
    local profile context
    local -i reexecute

    context=":chpwd:profiles:$PWD"
    zstyle -s "$context" profile profile || profile='default'
    zstyle -T "$context" re-execute && reexecute=1 || reexecute=0

    if (( ${+parameters[CHPWD_PROFILE]} == 0 )); then
        typeset -g CHPWD_PROFILE
        local CHPWD_PROFILES_INIT=1
        (( ${+functions[chpwd_profiles_init]} )) && chpwd_profiles_init
    elif [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_leave_profile_$CHPWD_PROFILE]} )) \
            && chpwd_leave_profile_${CHPWD_PROFILE}
    fi
    if (( reexecute )) || [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_profile_$profile]} )) && chpwd_profile_${profile}
    fi

    CHPWD_PROFILE="${profile}"
    return 0
}

chpwd_functions=( ${chpwd_functions} chpwd_profiles )

# Terminal-title wizardry

function ESC_print () {
    info_print $'\ek' $'\e\\' "$@"
}
function set_title () {
    info_print  $'\e]0;' $'\a' "$@"
}

function info_print () {
    local esc_begin esc_end
    esc_begin="$1"
    esc_end="$2"
    shift 2
    printf '%s' ${esc_begin}
    printf '%s' "$*"
    printf '%s' "${esc_end}"
}

# 'hash' some often used directories
#d# start
hash -d deb=/var/cache/apt/archives
hash -d doc=/usr/share/doc
hash -d linux=/lib/modules/$(command uname -r)/build/
hash -d log=/var/log
hash -d slog=/var/log/syslog
hash -d src=/usr/src
hash -d www=/var/www
#d# end

# if cdrecord is a symlink (to wodim) or isn't present at all warn:
if [[ -L /usr/bin/cdrecord ]] || ! check_com -c cdrecord; then
    if check_com -c wodim; then
        function cdrecord () {
            <<__EOF0__
cdrecord is not provided under its original name by Debian anymore.
See #377109 in the BTS of Debian for more details.

Please use the wodim binary instead
__EOF0__
            return 1
        }
    fi
fi


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

# Load completions
source "${ZDOTDIR}/.zshrc.completions"

# shell functions

# Reload an autoloadable function
function freload () {
    while (( $# )); do
        unfunction $1
        autoload -U $1
        shift;
    done
}
compdef _functions freload

# zsh profiling
function profile () {
    ZSH_PROFILE_RC=1 zsh "$@" || exit
}

# Edit an alias via zle
function edalias () {
    [[ -z "$1" ]] && { echo "Usage: edalias <alias_to_edit>" ; return 1 } || vared aliases'[$1]' ;
}
compdef _aliases edalias

# Edit a function via zle
function edfunc () {
    [[ -z "$1" ]] && { echo "Usage: edfunc <function_to_edit>" ; return 1 } || zed -f "$1" ;
}
compdef _functions edfunc

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

# smart cd function, allows switching to /etc when running 'cd /etc/fstab'
function cd () {
    if (( ${#argv} == 1 )) && [[ -f ${1} ]]; then
        [[ ! -e ${1:h} ]] && return 1
        print "Correcting ${1} to ${1:h}"
        builtin cd ${1:h}
    else
        builtin cd "$@"
    fi
}

# Colors
autoload colors
if [[ "$terminfo[colors]" -gt 8 ]]; then
    colors
fi

source "${ZDOTDIR}/.zshrc.keys"
source "${ZDOTDIR}/.zshrc.plugins"
source "${ZDOTDIR}/.zshrc.aliases"
source "${ZDOTDIR}/.zshrc.debian"
source "${ZDOTDIR}/.zshrc.arch"
source-if-exists "${HOME}/.zsh-system.zsh"

if is-profiling-zshrc; then
    float rc_end_time=${EPOCHREALTIME}
    float rc_elapsed_time=$(((rc_end_time - _RC_START_TIME) * 1000))
    print
    printf "% 3.0fms - Total\n" ${rc_elapsed_time}
    print
    print 'Use `zprof | less` for detailed results.'
fi

# Remove these functions again, they are of use only in these
# setup files. This should be called at the end of .zshrc.
function xunfunction () {
  emulate -L zsh
  local -a funcs
  local func
  # We might have overriden source and '.' for profiling.
  funcs=(salias xsource source . xunfunction zrcautoload zrcautozle)
  for func in $funcs ; do
    [[ -n ${functions[$func]} ]] \
      && unfunction $func
  done
  return 0
}


xunfunction
