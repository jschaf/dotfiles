#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

RESTORE='\033[0m'
RED='\033[00;31m'
GREEN='\033[00;32m'
YELLOW='\033[00;33m'
BLUE='\033[00;34m'

ok="${GREEN}  ok${RESTORE}"
warn="${YELLOW}  warn${RESTORE}"
error="${RED}  error${RESTORE}"

linkees=$(find "$DIR" -maxdepth 1 -depth 1 \
                                  ! -name setup-links.sh \
                                  ! -name \.             \
                                  ! -name .git           \
                                  ! -name .gitignore     \
                                  ! -name README)

echo "Linking files from $DIR..."

for linkee in $linkees; do
    source_name=$(basename "$linkee")
    target="$HOME/$source_name"
    # echo "trying to link $source_name to $target"

    # Check if target is already a symlink
    if [[ -h "$target" ]]; then

        existing_target=$(readlink "$target")
        existing_target_dir=$(dirname "$existing_target")

        if [[ "$existing_target_dir" == "$DIR" ]]; then
            echo "$ok: $source_name - already linked"

        else
            # echo "target_dir: '$existing_target_dir', dir: '$DIR'"
            echo "$error: $HOME/$source_name points to $existing_target"
        fi

    elif [[ -d "$target" ]]; then
        echo "$error: $target already exists as a directory"

    elif [[ -e "$target" ]]; then
        echo "$error: $target already exists as a file"

    elif ln -s "$DIR/$source_name" "$HOME"; then
        echo "$ok: $source_name - linked"

    else
        echo "$error: linking $source_name failed"
    fi

done
