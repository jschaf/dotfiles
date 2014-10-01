autoload -Uz compinit promptinit colors vcs_info
compinit
promptinit
colors

# History options
HISTFILE=~/.histfile
HISTSIZE=4096
SAVEHIST=4096

# Append to HISTFILE instead of overwrite
setopt appendhistory

# cd to a directory if it is given alone
setopt autocd

setopt extendedglob

setopt nomatch

# Don't beep
unsetopt beep notify

# Emacs key bindings
bindkey -e

zstyle :compinstall filename '/home/joe/.zshrc'

aliasFile="$HOME/.aliases"

if [ -f "${aliasFile}" ]; then
    source "${aliasFile}"
fi


zshrcAliasFile="$HOME/.zshrc.aliases"

if [ -f "${zshrcAliasFile}" ]; then
    source "${zshrcAliasFile}"
fi


zshrcFuncFile="$HOME/.zshrc.functions"

if [ -f "${zshrcFuncFile}" ]; then
    source "${zshrcFuncFile}"
fi

compdef '_files -g "*.gz *.tgz *.bz2 *.tbz *.zip *.rar *.tar *.lha"' extract_archive
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select

source $HOME/.shell-common.sh

# Replace $HOME with ~
function collapse_pwd {
    echo $(pwd | sed -e "s,^$HOME,~,")
}

# Allow for functions in the prompt.
setopt PROMPT_SUBST

zstyle ':vcs_info:*' stagedstr '%F{28}●'
zstyle ':vcs_info:*' unstagedstr '%F{11}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git svn darcs hg bzr

precmd () {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats ' [%F{bluen}%b%c%u]'
    } else {
        zstyle ':vcs_info:*' formats ' [%F{green}%b%c%u%F{red}●%F{white}]'
    }

    vcs_info
}

PROMPT='
%~${vcs_info_msg_0_}%{$reset_color%}
%F{green}%n@%m%{$reset_color%}%# '
