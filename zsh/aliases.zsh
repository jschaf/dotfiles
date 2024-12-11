#!/bin/zsh

# global aliases, dont have to be at the beginning of a line
alias -g H='| head'
alias -g T='| tail'
alias -g G='| rg'
alias -g L="| less"
alias -g LL="2>&1 | less"
alias -g CA="2>&1 | cat -A"
alias -g NE="2> /dev/null"
alias -g NUL="> /dev/null 2>&1"

alias ...='cd ../../'
alias ....='cd ../../../'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

alias doom='${HOME}/.emacs.d/bin/doom'

# we don't want to quote/espace URLs on our own...
# if autoload -U url-quote-magic ; then
#    zle -N self-insert url-quote-magic
#    zstyle ':url-quote-magic:*' url-metas '*?[]^()~#{}='
# else
#    print 'Notice: no url-quote-magic available :('
# fi
alias url-quote='autoload -U url-quote-magic; \
  zle -N self-insert url-quote-magic'

if [[ "$TERM" == dumb ]]; then
  alias la='command ls -la'
  alias ll='command ls -l'
  alias lh='command ls -hAl'
  alias l='command ls -l'
else
  if command-exists eza; then
    alias la='eza --long --git --all'
    alias ll='eza --long --git'
    alias lh='eza --long --git'
    alias l='eza --long --git'
  else
    alias ls="command ls ${ls_options:+${ls_options[*]}}"
    alias la="command ls -la ${ls_options:+${ls_options[*]}}"
    alias ll="command ls -l ${ls_options:+${ls_options[*]}}"
    alias lh="command ls -hAl ${ls_options:+${ls_options[*]}}"
    alias l="command ls -l ${ls_options:+${ls_options[*]}}"
  fi
fi

# General.
alias da='du -sch'
alias g='git'
alias rz='reload-zshrc'
alias rf='reload-function'
alias rfr='reload-function-and-run'
alias e='emacsclient --no-wait'
alias sll='symbolic-link-detail'
alias cdg='cd $(bazel-workspace-dir)'

# Work.
alias dimc='bazelisk run //dev/simc --'

# Bazel.
alias bazel='bazelisk'
alias blb='bazelisk build'
alias blt='bazelisk test'
alias blr='bazelisk run'
alias blq='bazelisk query'
# Update BUILD files when Go imports change.
alias blg='bazelisk run //build:gen --ui_event_filters=-info,-stdout,-stderr --noshow_progress'

# Listing stuff.
# Execute ls -lSrah
alias dir="command ls -lSrah"
# Only show dot-directories
alias lad='command ls -d .*(/)'
# Only show dot-files
alias lsa='command ls -a .*(.)'
# Only files with setgid/setuid/sticky flag
alias lss='command ls -l *(s,S,t)'
# Only show symlinks
alias lsl='command ls -l *(@)'
# Display only executables
alias lsx='command ls -l *(*)'
# Display world-{readable,writable,executable} files
alias lsw='command ls -ld *(R,W,X.^ND/)'
# Display the ten biggest files
alias lsbig="command ls -flh *(.OL[1,10])"
# Only show directories
alias lsd='command ls -d *(/)'
# Only show empty directories
alias lse='command ls -d *(/^F)'
# Display the ten newest files
alias lsnew="command ls -rtlh *(D.om[1,10])"
# Display the ten oldest files
alias lsold="command ls -rtlh *(D.Om[1,10])"
# Display the ten smallest files
alias lssmall="command ls -Srl *(.oL[1,10])"
# Display the ten newest directories and ten newest .directories
alias lsnewdir="command ls -rthdl *(/om[1,10]) .*(D/om[1,10])"
# Display the ten oldest directories and ten oldest .directories
alias lsolddir="command ls -rthdl *(/Om[1,10]) .*(D/Om[1,10])"

# ssh with StrictHostKeyChecking=no \\&\quad and UserKnownHostsFile unset
alias insecssh='ssh -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'
# scp with StrictHostKeyChecking=no \\&\quad and UserKnownHostsFile unset
alias insecscp='scp -o "StrictHostKeyChecking=no" -o "UserKnownHostsFile=/dev/null"'

# Remote hosts don't have xterm-24bit installed and use a really dumb terminal.
# Use xterm-256color as a reasonably full-featured terminal.
alias ssh='TERM=xterm ssh'

# Use colors with grep.
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"

alias b='bazelisk'
alias bazel='bazelisk'
alias blb='bazelisk build'
alias blr='bazelisk run'
alias blt='bazelisk test --test_output=errors'
alias blq='bazelisk query'

alias tf='terraform'

if is-linux; then
  alias open='xdg-open'
fi

if is-darwin; then
  alias new-emacs='open -n /Applications/Emacs.app --args'
fi

# --location follows redirects and --remote-name redirects input to
# a file with the same name as the server.
alias download='curl --fail --location --remote-name'
