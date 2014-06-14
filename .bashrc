
source "$HOME/.shell-common.sh"
source "$HOME/.git-prompt.sh"
shopt -s extglob

# Check for an interactive session
[ -z "$PS1" ] && return

function reload_bashrc {
    source ~/.bashrc
}

alias g=git

source /usr/local/bin/virtualenvwrapper.sh

check_virtualenv() {
    if [[ "$VIRTUAL_ENV" ]]; then
        return 0;
    fi
    possible_env="$(basename $(pwd))"
    if [[ -e "$WORKON_HOME/$possible_env" ]]; then
        workon "$possible_env"
    fi
}

venv_cd() {
    builtin cd "$@" && check_virtualenv
}
alias cd="venv_cd"

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

mac_randomize() {
    old_mac=$(ifconfig en0 | grep ether | cut -d\  -f2)
    local new_mac=$(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
    sudo ifconfig en0 ether $new_mac_address
    echo "MAC address changed from $old_mac to $new_mac"
}

# One tab key press shows completions if there are multiple
# completions
set show-all-if-ambiguous on
