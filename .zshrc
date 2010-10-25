autoload -Uz compinit promptinit
compinit
promptinit

prompt walters

export EDITOR="emacsclient"
export ALTERNATE_EDITOR="emacs"
export PATH="$HOME/bin:$HOME/.cabal/bin:$PATH"

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