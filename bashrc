#!/bin/bash


# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

dotfiles="$HOME/.dotfiles"

echo "loading .bashrc"
source "$dotfiles/shell-common.sh"
shopt -s extglob

function reload_bashrc {
    source ~/.bash_profile
}


function dotfile_install {
    (cd $HOME/.dotfiles && ./install)
}

include () {
    [[ -e "$1" ]] && source "$1"
}

include "$HOME/.bash_system.sh"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# Load RVM into a shell session *as a function*
include "$HOME/.rvm/scripts/rvm"

include "/usr/bin/virtualenvwrapper.sh"

# Use pip for system packages
syspip(){
   PIP_REQUIRE_VIRTUALENV="" pip "$@"
}

        RED="\[\033[0;31m\]"
     YELLOW="\[\033[1;33m\]"
      GREEN="\[\033[0;32m\]"
       BLUE="\[\033[1;34m\]"
    MAGENTA="\[\033[0;35m\]"
  LIGHT_RED="\[\033[1;31m\]"
LIGHT_GREEN="\[\033[1;32m\]"
      WHITE="\[\033[1;37m\]"
 LIGHT_GRAY="\[\033[0;37m\]"
 COLOR_NONE="\[\e[0m\]"

function color_my_prompt {
    local __user_and_host="$LIGHT_GREEN\u@\h$COLOR_NONE"
    local __cur_location="$BLUE\w$COLOR_NONE"
    local __prompt_tail="$YELLOW\$"
    local __last_color="\[\033[00m\]"


    export PS1="\n$__user_and_host $__cur_location\
$__prompt_tail$__last_color "
}

PROMPT_COMMAND=color_my_prompt

# One tab key press shows completions if there are multiple
# completions
bind "set show-all-if-ambiguous on"

# case-insensitive completion
bind "set completion-ignore-case on"

# color the man pages
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# append to the history file, don't overwrite it
shopt -s histappend

# Combine multiline commands into one in history
shopt -s cmdhist
HISTCONTROL=ignoreboth

export HISTIGNORE="&:ls:[bf]g:exit"

# Record each line as it gets issued
PROMPT_COMMAND="$PROMPT_COMMAND; history -a"

# Huge history. Doesn't appear to slow things down, so why not?
HISTSIZE=500000
HISTFILESIZE=100000

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands

export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history"

# Useful timestamp format
HISTTIMEFORMAT='%F %T '

# ignore case, long prompt, exit if it fits on one screen, allow
# colors for ls and grep colors
export LESS="-iMFXR"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# must press ctrl-D 2+1 times to exit shell
export IGNOREEOF="2"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [[ -x brew && -f $(brew --prefix)/etc/bash_completion ]]; then
        . $(brew --prefix)/etc/bash_completion
    elif [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

function updateProg {
    CUR_DIR=$(pwd)
    cd ~/prog
    find . -name .git -type d | xargs -n1 -P4 -I% git --git-dir=% --work-tree=%/.. remote update -p
    cd "$CUR_DIR"
}

function test-fonts-powerline() {
    printf "Powerline fonts glyps:\n"
    printf "\ue0b0 \u00b1 \ue0a0 \u27a6 \u2718 \u26a1 \u2699 \ue0b1 \ue0b2 \ue0b3\n"
}

function test-fonts-font-awesome() {
    printf "Font Awesome glyps:\n"
    printf "\uf2b4 \uf119 \uf1a0 \uf23b \uf087 \uf155\n"
}

function test-fonts() {
    test-fonts-powerline
    printf '\n'
    test-fonts-font-awesome
}


# added by travis gem
[ -f /home/joe/.travis/travis.sh ] && source /home/joe/.travis/travis.sh

alias ls='ls'
alias la='ls -lah'
alias lsd='ls -ld *(-/DN)'
alias lsa='ls -ld .*'
alias f='find |grep'
alias c="clear"
alias dir='ls -1'
alias ..='cd ..'
alias g=git
