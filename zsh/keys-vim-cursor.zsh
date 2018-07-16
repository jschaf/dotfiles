#!/bin/zsh

# Don't load cursor keys if we're a TTY.  The TTY can't handle
# different cursor types and outputs a raw 'q' character instead.
if is-tty; then
  return
fi

typeset -A keymaps_display
keymaps_display=(
  emacs "[emacs]"
  # When in vi mode, main is bound to viins
  main "%{$fg_bold[green]%}[ins]%{$reset_color%}"
  viins "%{$fg_bold[green]%}[ins]%{$reset_color%}"
  vicmd "%{$fg_bold[yellow]%}[cmd]%{$reset_color%}"
  visual "[visual]"
  isearch "[isearch]"
  command "[command]"
  .safe "[.safe]"
)

typeset -A cursor_types
cursor_types=(
  'blinking_block'          '\033[1 q'
  'solid_block'             '\033[2 q'
  'blinking_underbar'       '\033[3 q'
  'solid_underbar'          '\033[4 q'
  'blinking_vertical_bar'   '\033[5 q'
  'solid_vertical_bar'      '\033[6 q'
)
typeset -A cursor_by_keymap
cursor_by_keymap=(
  emacs $cursor_types[solid_block]
  main $cursor_types[solid_vertical_bar]
  viins $cursor_types[solid_vertical_bar]
  vicmd $cursor_types[solid_block]
  visual $cursor_types[solid_block]
  isearch $cursor_types[solid_block]
  command $cursor_types[solid_block]
  .safe $cursor_types[solid_block]
)

function widget-update-vim-prompt() {
  # RPS1="${keymaps_display[$KEYMAP]} ${cursor_by_keymap[$KEYMAP]}"
  # zle reset-prompt
}

zle -N widget-update-vim-prompt

function zle-line-init() {
  printf $cursor_by_keymap[$KEYMAP]
  zle -K ${_zsh_prev_vi_mode:-viins}
  # zle widget-update-vim-prompt
}

function zle-keymap-select() {
  printf $cursor_by_keymap[$KEYMAP]
  # zle widget-update-vim-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
