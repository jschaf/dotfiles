# Custom widgets:

# a generic accept-line wrapper

# This widget can prevent unwanted autocorrections from command-name
# to _command-name, rehash automatically on enter and call any number
# of builtin and user-defined widgets in different contexts.
#
# For a broader description, see:
# <http://bewatermyfriend.org/posts/2007/12-26.11-50-38-tooltime.html>
#
# The code is imported from the file 'zsh/functions/accept-line' from
# <http://ft.bewatermyfriend.org/comp/zsh/zsh-dotfiles.tar.bz2>, which
# distributed under the same terms as zsh itself.

# A newly added command will may not be found or will cause false
# correction attempts, if you got auto-correction set. By setting the
# following style, we force accept-line() to rehash, if it cannot
# find the first word on the command line in the $command[] hash.
zstyle ':acceptline:*' rehash true

function Accept-Line () {
  setopt localoptions noksharrays
  local -a subs
  local -xi aldone
  local sub
  local alcontext=${1:-$alcontext}

  zstyle -a ":acceptline:${alcontext}" actions subs

  (( ${#subs} < 1 )) && return 0

  (( aldone = 0 ))
  for sub in ${subs} ; do
    [[ ${sub} == 'accept-line' ]] && sub='.accept-line'
    zle ${sub}

    (( aldone > 0 )) && break
  done
}

function Accept-Line-getdefault () {
  emulate -L zsh
  local default_action

  zstyle -s ":acceptline:${alcontext}" default_action default_action
  case ${default_action} in
    ((accept-line|))
    printf ".accept-line"
    ;;
    (*)
      printf ${default_action}
      ;;
  esac
}

function Accept-Line-HandleContext () {
  zle Accept-Line

  default_action=$(Accept-Line-getdefault)
  zstyle -T ":acceptline:${alcontext}" call_default \
    && zle ${default_action}
}

function accept-line () {
  setopt localoptions noksharrays
  local -a cmdline
  local -x alcontext
  local buf com fname format msg default_action

  alcontext='default'
  buf="${BUFFER}"
  cmdline=(${(z)BUFFER})
  com="${cmdline[1]}"
  fname="_${com}"

  Accept-Line 'preprocess'

  zstyle -t ":acceptline:${alcontext}" rehash \
    && [[ -z ${commands[$com]} ]]           \
    && rehash

  if [[ -n ${com} ]] \
    && [[ -n ${reswords[(r)$com]} ]] \
    || [[ -n ${aliases[$com]} ]] \
    || [[ -n ${functions[$com]} ]] \
    || [[ -n ${builtins[$com]} ]] \
    || [[ -n ${commands[$com]} ]] ; then

    # there is something sensible to execute, just do it.
    alcontext='normal'
    Accept-Line-HandleContext

    return
  fi

  if [[ -o correct ]] \
    || [[ -o correctall ]] \
    && [[ -n ${functions[$fname]} ]] ; then

    # nothing there to execute but there is a function called
    # _command_name; a completion widget. Makes no sense to
    # call it on the commandline, but the correct{,all} options
    # will ask for it nevertheless, so warn the user.
    if [[ ${LASTWIDGET} == 'accept-line' ]] ; then
      # Okay, we warned the user before, he called us again,
      # so have it his way.
      alcontext='force'
      Accept-Line-HandleContext

      return
    fi

    if zstyle -t ":acceptline:${alcontext}" nocompwarn ; then
      alcontext='normal'
      Accept-Line-HandleContext
    else
      # prepare warning message for the user, configurable via zstyle.
      zstyle -s ":acceptline:${alcontext}" compwarnfmt msg

      if [[ -z ${msg} ]] ; then
        msg="%c will not execute and completion %f exists."
      fi

      zformat -f msg "${msg}" "c:${com}" "f:${fname}"

      zle -M -- "${msg}"
    fi
    return
  elif [[ -n ${buf//[$' \t\n']##/} ]] ; then
    # If we are here, the commandline contains something that is not
    # executable, which is neither subject to _command_name correction
    # and is not empty. might be a variable assignment
    alcontext='misc'
    Accept-Line-HandleContext

    return
  fi

  # If we got this far, the commandline only contains whitespace, or is empty.
  alcontext='empty'
  Accept-Line-HandleContext
}

zle -N accept-line
zle -N Accept-Line
zle -N Accept-Line-HandleContext
