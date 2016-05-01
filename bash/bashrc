#!/bin/bash

echo "loading .bashrc"
source "$HOME/.shell-common.sh"
source "$HOME/.git-prompt.sh"
shopt -s extglob

function reload_bashrc {
    source ~/.bash_profile
}

include () {
    [[ -f "$1" ]] && source "$1"
}

alias g=git

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
    local GIT_PS1_SHOWDIRTYSTATE='y'
    local GIT_PS1_SHOWCOLORHINTS='y'
    local __git=$(__git_ps1)
    # local __conda_color="\[\033[31m\]"
    local __conda_env='`echo $CONDA_DEFAULT_ENV | sed "s/..*/ (&)/"`'
    local __prompt_tail="$YELLOW\$"
    # Use test, otherwise basename shows it's help
    local __venv="$(test $VIRTUAL_ENV && basename $VIRTUAL_ENV)"
    # Use variable substitution ':+' to add parens and spaces
    local __venv_color="${__venv:+ ($GREEN$__venv$COLOR_NONE)}"
    local __last_color="\[\033[00m\]"


    export PS1="\n$__user_and_host $__cur_location\
$__conda_color$__conda_env\
$__venv_color\
$__git\n\
$__prompt_tail$__last_color "
}

PROMPT_COMMAND=color_my_prompt

extract() {
  local e=0 i c
  for i; do
    if [[ -r $i ]]; then
        c=''
        case $i in
          *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
                 c='bsdtar xvf' ;;
          *.7z)  c='7z x'       ;;
          *.Z)   c='uncompress' ;;
          *.bz2) c='bunzip2'    ;;
          *.exe) c='cabextract' ;;
          *.gz)  c='gunzip'     ;;
          *.rar) c='unrar x'    ;;
          *.xz)  c='unxz'       ;;
          *.zip) c='unzip'      ;;
          *)     echo "$0: cannot extract \`$i': Unrecognized file extension" >&2; e=1 ;;
        esac
        [[ $c ]] && command $c "$i"
    else
        echo "$0: cannot extract \`$i': File is unreadable" >&2; e=2
    fi
  done
  return $e
}

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

export HISTFILESIZE=20000
export HISTSIZE=10000
shopt -s histappend
# Combine multiline commands into one in history
shopt -s cmdhist
# Ignore duplicates, ls without options and builtin commands
HISTCONTROL=ignoredups
export HISTIGNORE="&:ls:[bf]g:exit"

# ignore case, long prompt, exit if it fits on one screen, allow
# colors for ls and grep colors
export LESS="-iMFXR"

# must press ctrl-D 2+1 times to exit shell
export IGNOREEOF="2"

include "$HOME/.bashrc-system"

function updateProg {
    CUR_DIR=$(pwd)
    cd ~/prog
    find . -name .git -type d | xargs -n1 -P4 -I% git --git-dir=% --work-tree=%/.. remote update -p
    cd "$CUR_DIR"
}

# added by travis gem
[ -f /home/joe/.travis/travis.sh ] && source /home/joe/.travis/travis.sh
