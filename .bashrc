
source "$HOME/.shell-common.sh"
source "$HOME/.git-prompt.sh"
shopt -s extglob

# Check for an interactive session
[ -z "$PS1" ] && return

function reload_bashrc {
    source ~/.bashrc
}

alias g=git

function color_my_prompt {
    local __user_and_host="\[\033[01;32m\]\u@\h"
    local __cur_location="\[\033[01;34m\]\w"
    local GIT_PS1_SHOWDIRTYSTATE='y'
    local GIT_PS1_SHOWCOLORHINTS='y'
    local __git=$(__git_ps1)
    # local __conda_color="\[\033[31m\]"
    local __conda_env='`echo $CONDA_DEFAULT_ENV | sed "s/..*/ (&)/"`'
    local __prompt_tail="\[\033[35m\]\$"
    local __last_color="\[\033[00m\]"

    export PS1="\n$__user_and_host $__cur_location\
$__conda_color$__conda_env\
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
set show-all-if-ambiguous on
