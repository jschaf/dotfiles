# Claude Code session environment (pointed to by $CLAUDE_ENV_FILE in .zshenv).
#
# The Bash tool sources this before every command, AFTER its shell snapshot
# restores a PATH captured from the launching app's environment. That snapshot
# clobbers the shims prepend .zshenv makes, so a Claude session whose desktop
# app inherited a stale PATH resolves pinned tools (node, pnpm) to the system
# ones — and `mise exec` cannot rescue it, since it no-ops whenever the shims
# dir is already anywhere in PATH. This preamble re-asserts the prepend where
# it wins. It runs for every Bash command, so it must stay idempotent.
case "$PATH" in
"$HOME/.local/share/mise/shims:"*) ;;
*) export PATH="$HOME/.local/share/mise/shims:$PATH" ;;
esac
