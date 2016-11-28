#!/bin/zsh

# Google specific functionality.

# A Snippet Helper (ASH).  http://go/asnippethelper	
alias ash=/google/data/ro/projects/toolshed/ash.par

function my_get_snippets() {
  ssh jschaf "/google/data/ro/projects/toolshed/ash.par --critique --calendar \
    --event_status=accepted --mark_down --pending_cls --bugs --assigned "
}
