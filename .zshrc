autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

export EDITOR="emacsclient"
export ALTERNATE_EDITOR="emacs"

# History options
HISTFILE=~/.histfile
HISTSIZE=4000
SAVEHIST=4000

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

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/joe/.zshrc'

# End of lines added by compinstall

# Normal aliases
alias ls='ls --color=auto'
alias lsd='ls -ld *(-/DN)'
alias lsa='ls -ld .*'
alias f='find |grep'
alias c="clear"
alias dir='ls -1'
alias ..='cd ..'


extract_archive () {
    local old_dirs current_dirs lower
    lower=${(L)1}
    old_dirs=( *(N/) )
    if [[ $lower == *.tar.gz || $lower == *.tgz ]]; then
        tar zxfv $1
    elif [[ $lower == *.gz ]]; then
        gunzip $1
    elif [[ $lower == *.tar.bz2 || $lower == *.tbz ]]; then
        bunzip2 -c $1 | tar xfv -
    elif [[ $lower == *.bz2 ]]; then
        bunzip2 $1
    elif [[ $lower == *.zip ]]; then
        unzip $1
    elif [[ $lower == *.rar ]]; then
        unrar e $1
    elif [[ $lower == *.tar ]]; then
        tar xfv $1
    elif [[ $lower == *.lha ]]; then
        lha e $1
    else
        print "Unknown archive type: $1"
        return 1
    fi
    # Change in to the newly created directory, and
    # list the directory contents, if there is one.
    current_dirs=( *(N/) )
    for i in {1..${#current_dirs}}; do
        if [[ $current_dirs[$i] != $old_dirs[$i] ]]; then
            cd $current_dirs[$i]
            ls
            break
        fi
    done
}

alias ex=extract_archive
compdef '_files -g "*.gz *.tgz *.bz2 *.tbz *.zip *.rar *.tar *.lha"' extract_archive