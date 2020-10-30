#!/bin/zsh

# Completion system
COMPDUMPFILE=${ZDOTDIR}/.zcompdump
if autoload compinit ; then
  compinit -d ${COMPDUMPFILE} -C || print 'Notice: no compinit available :('
else
  print 'Notice: no compinit available :('
  function compdef { }
fi

# Enables shell command completion for gcloud.
if [[ -f '/d/google-cloud-sdk/completion.zsh.inc' ]]; then
  . '/d/google-cloud-sdk/completion.zsh.inc'
fi

# The number of items to list without asking first.  0 means show them all.  We
# use a pager via complist, so I'd rather zsh just show completions immediately.
LISTMAX=0

# Add completions for autoloaded functions.
#
# This must come after compinit and we want it only for interactive sessions
# since it take ~40ms.  We can't put these compdef calls inside the autoloaded
# function definitions because the functions are autoloaded in zshenv.  There
# might be a way to use a standalone compdef but it doesn't seem worth the
# trouble.
compdef _functions edit-function
compdef _aliases edit-alias
compdef _functions reload-function

# Note: use 'zstyle' for getting current settings.
# Use ^xh (control-x h) for getting tags in context.
# Use ^x? (control-x ?) to run complete_debug with trace output.

# Make sure the completion system is initialised.
(( ${+_comps} )) || print-error "Completion not enabled."

# Allow one error for every three characters typed in approximate completer.
zstyle ':completion:*:approximate:'    max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

# Don't complete backup files as executables.
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

# Start menu completion only if it could find an ambiguous initial string.
zstyle ':completion:*:correct:*'       insert-unambiguous true
zstyle ':completion:*:corrections'     format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
zstyle ':completion:*:correct:*'       original true

# Activate color-completion.
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

# Format on completion.
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

# Automatically complete 'cd -<tab>' and 'cd -<ctrl-d>' with menu.
# zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# Insert all expansions for expand completer.
zstyle ':completion:*:expand:*'        tag-order all-expansions
zstyle ':completion:*:history-words'   list false

# Activate menu.
zstyle ':completion:*:history-words'   menu yes

# Ignore duplicate entries.
zstyle ':completion:*:history-words'   remove-all-dups yes
zstyle ':completion:*:history-words'   stop yes

# Match uppercase from lowercase.
zstyle ':completion:*'                 matcher-list 'm:{a-z}={A-Z}'

# Separate matches into groups.
zstyle ':completion:*:matches'         group 'yes'
zstyle ':completion:*'                 group-name ''

# Always use menu completion.
zstyle ':completion:*'               menu select=0

zstyle ':completion:*:messages'        format '%d'
zstyle ':completion:*:options'         auto-description '%d'

# Describe options in full.
zstyle ':completion:*:options'         description 'yes'

# On processes completion complete all user processes.
zstyle ':completion:*:processes'       command 'ps -au$USER'

# Offer indexes before parameters in subscripts.
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Provide verbose completion information.
zstyle ':completion:*'                 verbose true

# recent (as of Dec 2007) zsh versions are able to provide descriptions
# for commands (read: 1st word in the line) that it will list for the user
# to choose from. The following disables that, because it's not exactly fast.
zstyle ':completion:*:-command-:*:'    verbose false

# Set format for warnings.
zstyle ':completion:*:warnings'        format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'

# Define files to ignore for zcompile.
zstyle ':completion:*:*:zcompile:*'    ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:'          prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:.
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:.
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# Complete manual by their section.
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# Search path for sudo completion.
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
  /usr/local/bin  \
  /usr/sbin       \
  /usr/bin        \
  /sbin           \
  /bin            \
  /usr/X11R6/bin

# Provide .. as a completion.
zstyle ':completion:*' special-dirs ..

# Run rehash on completion so new installed program are found automatically:.
function _force_rehash () {
  (( CURRENT == 1 )) && rehash
  return 1
}

# Try to be smart about when to use what completer....
setopt correct
zstyle -e ':completion:*' completer '
        if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]] ; then
            _last_try="$HISTNO$BUFFER$CURSOR"
            reply=(_complete _match _ignored _prefix _files)
        else
            if [[ $words[1] == (rm|mv) ]] ; then
                reply=(_complete _files)
            else
                reply=(_oldlist _expand _force_rehash _complete _ignored _correct _approximate _files)
            fi
        fi'

# Command for process lists, the local web server details and host completion.
zstyle ':completion:*:urls' local 'www' '/var/www/' 'public_html'

# Some functions, like _apt and _dpkg, are very slow. We can use a cache in
# order to speed things up.
COMPLETION_CACHE_DIR=${ZDOTDIR}/.cache
mkdir -p "${COMPLETION_CACHE_DIR}"
zstyle ':completion:*' use-cache  yes
zstyle ':completion:*:complete:*' cache-path "${COMPLETION_CACHE_DIR}"

# Host completion.
[[ -r ~/.ssh/config ]] && _ssh_config_hosts=(${${(s: :)${(ps:\t:)${${(@M)${(f)"$(<$HOME/.ssh/config)"}:#Host *}#Host }}}:#*[*?]*}) || _ssh_config_hosts=()
[[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
  $(hostname)
  "$_ssh_config_hosts[@]"
  "$_ssh_hosts[@]"
  "$_etc_hosts[@]"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts

# Use generic completion system for programs not yet defined; (_gnu_generic
# works with commands that provide a --help option with "standard" gnu-like
# output).
for compcom in cp deborphan df feh fetchipac gpasswd head hnb ipacsum mv \
  pal stow uname ; do
  [[ -z ${_comps[$compcom]} ]] && compdef _gnu_generic ${compcom}
done; unset compcom

# See upgrade function in this file.
compdef _hosts upgrade

function rebuild_completion() {
  rm "${COMPDUMPFILE}"
  compinit
}

