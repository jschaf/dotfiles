#!/bin/zsh

export LANG=en_US.UTF-8
# Set CLICOLOR if you want Ansi Colors in iTerm2
export CLICOLOR=1
export TERM="xterm-256color"
export ALTERNATE_EDITOR="emacs"
export EDITOR="emacsclient -a emacs"
export VISUAL="emacsclient"
export WORKON_HOME="$HOME/.dotfiles/home/.virtualenvs"
export PROJECT_HOME="$HOME/prog"

INIT_LOG_FILE="${HOME}/.zsh-init-log"


function setup-init-log() {
    if [[ -f "${INIT_LOG_FILE}" ]]; then
        touch "${INIT_LOG_FILE}"
    fi

    # Clear the file
    echo -n '' > "${INIT_LOG_FILE}"

}


function init-log() {
    printf "%s\n" "$1" >> "${INIT_LOG_FILE}"
}


function include () {
    if [[ -e "$1" ]]; then
        source "$1"
        init-log "Sourced $1"
    else
        init-log "WARNING: didn't source $1"
    fi
}

function setup-zgen() {
    include "${HOME}/.config/zgen/zgen.zsh"

    if ! zgen saved; then

        # specify plugins here
        zgen load zsh-users/zsh-syntax-highlighting

        # k is a zsh script / plugin to make directory listings more readable,
        # adding a bit of color and some git status information on files and
        # directories.
        zgen load supercrabtree/k

        # Type in any part of any previously entered command and press the UP
        # and DOWN arrow keys to cycle through the matching commands.  Substring
        # search must load search after zsh-syntax-highlighting.
        zgen load zsh-users/zsh-history-substring-search

        # generate the init script from plugins above
        zgen save
    fi
}


function add-to-path () {
    if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
            init-log "Added $1 to path at back.  Path is now: $PATH"
        else
            PATH=$1:$PATH
            init-log "Added $1 to path at front.  Path is now: $PATH"
        fi
    fi
}


function add-to-path-if-exists() {
    if [[ -d "$1" ]]; then
        add-to-path "$1" "$2"
    else
        init-log "WARNING: didn't add to \$PATH because path doesn't exist: $1"
    fi
}


function setup-GPG() {
    GPG_TTY=$(tty)
    export GPG_TTY
    init-log "Exported \$GPG_TTY as $GPG_TTY"
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


function reload-zshrc() {
    source ~/.zshrc
}


# Package Setup

function setup-zsh-history-substring-search() {
    # OPTION 1: for most systems
    zmodload zsh/terminfo
    bindkey "$terminfo[kcuu1]" history-substring-search-up
    bindkey "$terminfo[kcud1]" history-substring-search-down

    # OPTION 2: for iTerm2 running on Apple MacBook laptops
    zmodload zsh/terminfo
    bindkey "$terminfo[cuu1]" history-substring-search-up
    bindkey "$terminfo[cud1]" history-substring-search-down

    ## EMACS mode ###########################################
    bindkey -M emacs '^P' history-substring-search-up
    bindkey -M emacs '^N' history-substring-search-down
}



setup-init-log
setup-zgen


# * Path Setup
#
# View adding paths without a second arg of after like a stack, so the last
# entry is the first directory searched for executables.
add-to-path-if-exists "/usr/share/texmf-dist/scripts/texlive"
add-to-path "/usr/local/bin"
add-to-path-if-exists "/usr/local/sbin"
add-to-path-if-exists "$HOME/homebrew/bin"

# Add coreutils to path
command -v brew >/dev/null 2>&1 && [ -d "$(brew --prefix coreutils)/libexec/gnubin" ] && \
    add-to-path "$(brew --prefix coreutils)/libexec/gnubin"

add-to-path "$HOME/bin"
add-to-path "$HOME/bin-system"



setup-GPG
setup-zsh-history-substring-search


alias rz='reload-zshrc'
alias kl='k -l'
alias ka='k -a'
alias kh='k -h'

include "${HOME}/.zsh_system.sh"
