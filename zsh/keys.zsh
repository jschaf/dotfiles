#!/bin/zsh

# Keyboard setup: The following is based on the same code, we wrote for
# debian's setup. It ensures the terminal is in the right mode, when zle is
# active, so the values from $terminfo are valid. Therefore, this setup should
# work on all systems, that have support for `terminfo'. It also requires the
# zsh in use to have the `zsh/terminfo' module built.
#
# If you are customising your `zle-line-init()' or `zle-line-finish()'
# functions, make sure you call the following utility functions in there:
#
#     - zle-line-init():      zle-smkx
#     - zle-line-finish():    zle-rmkx


bindkey -v # Link viins to main.

# Hundredths of a second (1=10ms).  Default is 40 (400ms).
KEYTIMEOUT=10

function widget-autoload-register() {
  emulate -L zsh
  local widget=$1
  autoload $widget && zle -N $widget
}

function autoload-widgets-in-dir() {
  local autoload_dir="$1"
  fpath+="${autoload_dir}"

  # Autoload all shell functions from in a given directory that have
  # the executable bit set.  The executable bit is not necessary, but
  # gives you an easy way to stop the autoloading of a particular
  # shell function.
  for widget in ${autoload_dir}/*(N-.x:t); do
    widget-autoload-register "${widget}"
  done
}

autoload-widgets-in-dir "${ZSH_DOTFILES}/widgets"

zle -N beginning-of-somewhere widget-beginning-or-end-of-somewhere
zle -N end-of-somewhere widget-beginning-or-end-of-somewhere
zle -N bracketed-paste widget-bracketed-paste

# tmux completion
zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_words_current_window
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

# Load a few more functions and tie them to widgets, so they can be bound:

function keymap-exists () {
  [[ -n ${(M)keymaps:#$1} ]]
}

widget-autoload-register insert-files
widget-autoload-register edit-command-line
widget-autoload-register insert-unicode-char
if autoload history-search-end; then
  zle -N history-beginning-search-backward-end history-search-end
  zle -N history-beginning-search-forward-end  history-search-end
fi
zle -C hist-complete complete-word _generic
zstyle ':completion:hist-complete:*' completer _history

# The actual terminal setup hooks and bindkey-calls:

# Binds a key-binding in provided maps.
# Uses all maps until '--' followed by a key and function.
# bind-maps emacs viins -- '^x.' widget-insert-abbreviation
function bind-maps() {
  local i sequence widget
  local -a maps

  while [[ "$1" != "--" ]]; do
    maps+=( "$1" )
    shift
  done

  # Remove '--'.
  shift
  sequence="$1"
  widget="$2"
  [[ -z "$sequence" ]] && return 1
  for map in "${maps[@]}"; do
    bindkey -M "$map" "$sequence" "$widget"
  done
}

function bind-maps-by-key-name() {
  local i sequence widget
  local -a maps
  while [[ "$1" != "--" ]]; do
    maps+=( "$1" )
    shift
  done

  # Remove '--'.
  shift
  sequence="${key[$1]}"
  widget="$2"
  [[ -z "$sequence" ]] && return 1
  for map in "${maps[@]}"; do
    bindkey -M "$map" "$sequence" "$widget"
  done
}

typeset -A key
key=(
  Home     "${terminfo[khome]}"
  End      "${terminfo[kend]}"
  Insert   "${terminfo[kich1]}"
  Delete   "${terminfo[kdch1]}"
  Up       "${terminfo[kcuu1]}"
  Down     "${terminfo[kcud1]}"
  Left     "${terminfo[kcub1]}"
  Right    "${terminfo[kcuf1]}"
  PageUp   "${terminfo[kpp]}"
  PageDown "${terminfo[knp]}"
  BackTab  "${terminfo[kcbt]}"
)

# Guidelines for adding key bindings:
#
#   - Do not add hardcoded escape sequences, to enable non standard key
#     combinations such as Ctrl-Meta-Left-Cursor. They are not easily portable.
#
#   - Adding Ctrl characters, such as '^b' is okay; note that '^b' and '^B' are
#     the same key.
#
#   - All keys from the $key[] mapping are obviously okay.
#
#   - Most terminals send "ESC x" when Meta-x is pressed. Thus, sequences like
#     '\ex' are allowed in here as well.

bind-maps-by-key-name emacs             -- Home   beginning-of-line
bind-maps-by-key-name       viins vicmd -- Home   vi-beginning-of-line
bind-maps-by-key-name emacs             -- End    end-of-somewhere
bind-maps-by-key-name       viins vicmd -- End    vi-end-of-line
bind-maps-by-key-name emacs viins       -- Insert overwrite-mode
bind-maps-by-key-name             vicmd -- Insert vi-insert
bind-maps-by-key-name emacs             -- Delete delete-char
bind-maps-by-key-name       viins vicmd -- Delete vi-delete-char
bind-maps-by-key-name emacs viins vicmd -- Up     up-history
bind-maps-by-key-name emacs viins vicmd -- Down   down-history
bind-maps-by-key-name emacs             -- Left   backward-char
bind-maps-by-key-name       viins vicmd -- Left   vi-backward-char
bind-maps-by-key-name emacs             -- Right  forward-char
bind-maps-by-key-name       viins vicmd -- Right  vi-forward-char
# Perform abbreviation expansion
bind-maps emacs viins       -- '^x.' widget-insert-abbreviation
# Display list of abbreviations that would expand
bind-maps emacs viins       -- '^xb' help-show-abk
# mkdir -p <dir> from string under cursor or marked area
bind-maps emacs viins       -- '^xM' widget-inplace-mkdirs
# display help for keybindings and ZLE
bind-maps emacs viins       -- '^xz' help-zle
# Insert files and test globbing
bind-maps emacs viins       -- "^xf" insert-files
# Edit the current line in $EDITOR
bind-maps emacs viins vicmd -- '^x^e' edit-command-line
# search history backward for entry beginning with typed text
bind-maps emacs viins       -- '^xp' history-beginning-search-backward-end
# search history forward for entry beginning with typed text
bind-maps emacs viins       -- '^xP' history-beginning-search-forward-end
# search history backward for entry beginning with typed text
bind-maps-by-key-name emacs viins       -- PageUp history-beginning-search-backward-end
# search history forward for entry beginning with typed text
bind-maps-by-key-name emacs viins       -- PageDown history-beginning-search-forward-end
bind-maps emacs viins       -- "^x^h" widget-commit-to-history
# Kill left-side word or everything up to next slash
bind-maps emacs viins       -- '\ev' widget-slash-backward-kill-word
# Kill left-side word or everything up to next slash
bind-maps emacs viins       -- '\e^h' widget-slash-backward-kill-word
# Kill left-side word or everything up to next slash
bind-maps emacs viins       -- '\e^?' widget-slash-backward-kill-word
# Do history expansion on space:
bind-maps emacs viins       -- ' ' magic-space
# ^@ is ctrl-space
bind-maps emacs viins       -- '^@' widget-expand-everything
# Trigger menu-complete
bind-maps emacs viins       -- '\ei' menu-complete  # menu completion via esc-i
# Insert a timestamp on the command line (yyyy-mm-dd)
# TODO: This makes C-e really slow, like 600ms
# bind-maps emacs viins       -- '^ed' widget-insert-datestamp
# Insert last typed word
bind-maps emacs viins       -- "\em" widget-insert-last-typed-word
# A smart shortcut for fg<enter>
bind-maps emacs viins       -- '^z' widget-jobs-fg
# prepend the current command with "sudo"
bind-maps emacs viins vicmd     -- "^os" widget-sudo-command-line
# jump to after first word (for adding options)
bind-maps emacs viins       -- '^x1' widget-jump-after-first-word
# complete word from history with menu
bind-maps emacs viins       -- "^x^x" hist-complete

# insert unicode character
# usage example: 'ctrl-x i' 00A7 'ctrl-x i' will give you an §
# See for example http://unicode.org/charts/ for unicode characters code
# Insert Unicode character
bind-maps emacs viins       -- '^xi' insert-unicode-char

if keymap-exists menuselect; then
  # Shift-tab Perform backwards menu completion
  bind-maps-by-key-name menuselect -- BackTab reverse-menu-complete

  # menu selection: pick item but stay in the menu
  bind-maps menuselect -- '\e^M' accept-and-menu-complete
  # also use + and INSERT since it's easier to press repeatedly
  bind-maps menuselect -- '+' accept-and-menu-complete
  bind-maps-by-key-name menuselect -- Insert accept-and-menu-complete

  # accept a completion and try to complete again by using menu
  # completion; very useful with completing directories
  # by using 'undo' one's got a simple file browser
  bind-maps menuselect -- '^o' accept-and-infer-next-history

  # ^@ is \C-<space>
  bind-maps menuselect -- '^@' accept-and-menu-complete
fi

## use Ctrl-left-arrow and Ctrl-right-arrow for jumping to word-beginnings on
## the command line.
# URxvt sequences:
typeset -a keys_all_modes
# Default for Emacs, but missing in viins and vicmd.
bind-maps emacs viins vicmd -- '\C-a' beginning-of-line
bind-maps emacs viins vicmd -- '\C-e' end-of-line
bind-maps emacs viins vicmd -- '\C-k' kill-line
bind-maps emacs viins vicmd -- '\C-u' kill-whole-line

# Missing from viins
bind-maps emacs viins vicmd -- '\C-p' up-history
bind-maps emacs viins vicmd -- '\C-n' down-history
bind-maps emacs viins vicmd -- '\C-?' backward-delete-char
bind-maps emacs viins vicmd -- '\C-h' backward-delete-char
bind-maps emacs viins vicmd -- '\e.' insert-last-word

bind-maps emacs viins vicmd -- '\eOc' forward-word
bind-maps emacs viins vicmd -- '\eOd' backward-word
# These are for xterm:
bind-maps emacs viins vicmd -- '\e[1;5C' forward-word
bind-maps emacs viins vicmd -- '\e[1;5D' backward-word
## the same for alt-left-arrow and alt-right-arrow
# URxvt again:
bind-maps emacs viins vicmd -- '\e\e[C' forward-word
bind-maps emacs viins vicmd -- '\e\e[D' backward-word
# Xterm again:
bind-maps emacs viins vicmd -- '^[[1;3C' forward-word
bind-maps emacs viins vicmd -- '^[[1;3D' backward-word
# Also try ESC Left/Right:
bind-maps emacs viins vicmd -- '\e'${key[Right]} forward-word
bind-maps emacs viins vicmd -- '\e'${key[Left]}  backward-word

# Ctrl-Delete, the preferred '^'${key[Delete]} didn't work
bind-maps emacs viins vicmd -- '^[[3;5~'  delete-word

bind-maps emacs viins vicmd -- '^x^f' widget-select-file
bind-maps emacs viins vicmd -- '^x^p' widget-select-bazel-package
bind-maps emacs viins vicmd -- '^x^x' widget-select-command
bind-maps emacs viins vicmd -- '^x^g' widget-select-word-from-current-tmux-window

# Spacemacs style bindings.
# Unbind space first.
bindkey -M vicmd -r ' '
bind-maps vicmd -- ' pf' widget-select-file
bind-maps vicmd -- ' pd' widget-select-directory
bind-maps vicmd -- ' pc' widget-select-bazel-package
bind-maps vicmd -- ' pb' widget-select-bazel-binary
bind-maps vicmd -- ' st' widget-select-word-from-current-tmux-window
bind-maps vicmd -- '  ' widget-select-command

bind-maps vicmd -- ' go' widget-select-hg-commits
bind-maps vicmd -- ' gc' widget-checkout-hg-commit


bind-maps vicmd -- ' w/' widget-tmux-split-window-horizontal
bind-maps vicmd -- ' w-' widget-tmux-split-window-vertical

bind-maps vicmd -- ' wk' widget-tmux-select-pane-up
bind-maps vicmd -- ' wj' widget-tmux-select-pane-down
bind-maps vicmd -- ' wh' widget-tmux-select-pane-left
bind-maps vicmd -- ' wl' widget-tmux-select-pane-right

bind-maps vicmd -- ' sf' widget-select-tmux-pane-files
bind-maps vicmd -- ' su' widget-select-tmux-pane-urls

bind-maps vicmd -- ' yt' widget-copy-last-tmux-output-with-prompt
bind-maps vicmd -- ' yT' widget-copy-last-tmux-output


# Suckless terminal seems to bind this by default, leave it just in case.
bind-maps emacs viins vicmd -- '\C-V' widget-paste-from-clipboard
bind-maps emacs viins vicmd -- '\C-y' widget-paste-from-clipboard


bind-maps emacs viins vicmd -- '^r' widget-select-history

# Just viins.
bind-maps viins -- 'jk' vi-cmd-mode
bind-maps viins -- 'kj' vi-cmd-mode
bind-maps viins -- '\C-xp' widget-select-command

bind-maps vicmd -- 'H' beginning-of-line
bind-maps vicmd -- 'L' end-of-line

# Keyboard shortcuts.

# Store the current command and use it as the initial text for following
# commands.
bind-maps emacs viins vicmd -- '^[q' push-input

# Fix binding for delete key.
bind-maps emacs viins vicmd -- '^[[P' delete-char

# Add bindings for completions
bind-maps emacs viins vicmd -- '^xt' tmux-pane-words-prefix
bind-maps emacs viins vicmd -- '\e/' tmux-pane-words-anywhere

source "${ZSH_DOTFILES}/keys-vim-cursor.zsh"

function zle-smkx () {}
function zle-rmkx () {}
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-smkx () {
    emulate -L zsh
    printf '%s' ${terminfo[smkx]}
  }
  function zle-rmkx () {
    emulate -L zsh
    printf '%s' ${terminfo[rmkx]}
  }
else
  (( ${+terminfo[smkx]} ))
  (( ${+terminfo[rmkx]} ))
fi


function zle-line-init() {
  # Update the cursor for the initial keymap.
  echo -n $cursor_by_keymap[$KEYMAP]

  # Enter application mode so bindings for home, end, arrow keys and function
  # keys work.
  zle-smkx
}

function zle-keymap-select() {
  # Update the cursor for the current keymap.
  echo -n $cursor_by_keymap[$KEYMAP]
}

function zle-line-finish () {
  # Exit application mode.
  zle-rmkx
}

zle -N zle-line-init
zle -N zle-keymap-select
zle -N zle-line-finish
