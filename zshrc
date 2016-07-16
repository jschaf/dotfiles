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

# If this matches the current user, then user@host isn't displayed
DEFAULT_USER='joe'
if [[ "${HOST}" =~ .*corp.google.com ]]; then
    DEFAULT_USER='jschaf'
fi

include () {
    [[ -e "$1" ]] && source "$1"
}

include "${HOME}/.config/zgen/zgen.zsh"

if ! zgen saved; then

    # specify plugins here
    # zgen prezto
    zgen load zsh-users/zsh-syntax-highlighting

    # generate the init script from plugins above
    zgen save
fi

pathmunge () {
    if ! echo $PATH | grep -Eq "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
        else
            PATH=$1:$PATH
        fi
    fi
}

pathmunge-if-exists() {
    [[ -d "$1" ]] && pathmunge "$1" "$2"
}

path_remove ()  {
    PATH=`echo -n $PATH | awk -v RS=: -v ORS=: '$0 != "'$1'"' | sed 's/:$//'`;
}

# like a stack, so the last entry is the first directory searched for
# executables.
pathmunge-if-exists "/usr/share/texmf-dist/scripts/texlive"
pathmunge "/usr/local/bin"
pathmunge-if-exists "/usr/local/sbin"
pathmunge-if-exists "$HOME/homebrew/bin"

# Add coreutils to path
command -v brew >/dev/null 2>&1 && [ -d "$(brew --prefix coreutils)/libexec/gnubin" ] && \
    pathmunge "$(brew --prefix coreutils)/libexec/gnubin"

pathmunge "$HOME/bin"
pathmunge "$HOME/bin-system"

GPG_TTY=$(tty)
export GPG_TTY

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

function reload-zshrc {
    source ~/.zshrc
}


include "${HOME}/.zsh_system.sh"
