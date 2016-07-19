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

autoload colors
if [[ "$terminfo[colors]" -gt 8 ]]; then
    colors
fi

for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
    # wrap colours between %{ %} to avoid weird gaps in autocomplete
    eval $COLOR='%{$fg_no_bold[${(L)COLOR}]%}'
    eval BOLD_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
eval RESET='%{$reset_color%}'

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
        init-log "Sourcing $1"
        source "$1"
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

        # More completion files for zsh.
        zgen load zsh-users/zsh-completions src

        # A Zsh plugin to help remembering those shell aliases and Git aliases
        # you once defined.  Prints a help line reminding you of an alias for a
        # command you typed.
        zgen load djui/alias-tips

        # Type in any part of any previously entered command and press the UP
        # and DOWN arrow keys to cycle through the matching commands.  Substring
        # search must load search after zsh-syntax-highlighting.
        zgen load zsh-users/zsh-history-substring-search

        # With zsh-async you can run multiple asynchronous jobs, enforce unique
        # jobs (multiple instances of the same job will not run), flush all
        # currently running jobs and create multiple workers (each with their
        # own jobs). For each worker you can register a callback-function
        # through which you will be notified about the job results (job name,
        # return code, output and execution time).
        zgen load mafredri/zsh-async

        zgen load rupa/z

        # generate the init script from plugins above
        zgen save
    fi
}

function contains() {
    string="$1"
    substring="$2"
    if test "${string#*$substring}" != "$string"
    then
        return 0    # $substring is in $string
    else
        return 1    # $substring is not in $string
    fi
}

function add-to-colon-separated-env-var() {
    local envKey="$1"
    local envValue="${(P)1}"
    local pathToAdd="$2"
    local insertLocation="$3"

    if ! contains "${envValue}" "${pathToAdd}" ; then
        if [ "${insertLocation}" = "after" ] ; then
            eval "${envKey}=${envValue}:${pathToAdd}"
            init-log "Added ${pathToAdd} to \$${envKey} at back.  \$${envKey} is now: ${(P)envKey}"
        else
            eval "${envKey}=${pathToAdd}:${envValue}"
            init-log "Added ${pathToAdd} to \$${envKey} at back.  \$${envKey} is now: ${(P)envKey}"
        fi
    fi
}

function add-to-path () {
    add-to-colon-separated-env-var PATH "$1" "$2"
}

function add-to-manpath() {
    add-to-colon-separated-env-var MANPATH "$1" "$2"
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

function reload-zshrc() {
    source ~/.zshrc
}

function setup-prompt() {
    include "${HOME}/.config/zsh/prompt.zsh"
}

function reload-prompt() {
    include "${HOME}/.config/zsh/prompt.zsh"
}

function setup-personal-packages() {
    include "${HOME}/.config/zsh/extract.zsh"
    include "${HOME}/.config/zsh/spectrum.zsh"
}


# Package Setup

# Set keystrokes for substring searching
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

# fzf is a general-purpose command-line fuzzy finder.
function setup-fzf() {
    local fzfPath="${HOME}/.dotfiles/vendor/fzf"
    add-to-path "${fzfPath}/bin" after
    add-to-manpath "${fzfPath}/man" after

    # Auto-completion
    [[ $- == *i* ]] && include "${fzfPath}/shell/completion.zsh" 2> /dev/null

    # Key bindings
    include "${fzfPath}/shell/key-bindings.zsh"
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
setup-prompt
setup-personal-packages
setup-fzf

alias g='git'
alias gRl='git remote --verbose'

alias kl='k -l'
alias ka='k -a'
alias kh='k -h'

alias rz='reload-zshrc'
alias rp='reload-prompt'

include "${HOME}/.zsh-system.zsh"
