#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

RESTORE='\033[0m'
RED='\033[00;31m'
GREEN='\033[00;32m'
YELLOW='\033[00;33m'
BLUE='\033[00;34m'

ok="${GREEN}    ok${RESTORE}"
skip="  skip"
warn="${YELLOW}  warn${RESTORE}"
error="${RED} error${RESTORE}"

linkees=$(find "$DIR" -maxdepth 1  \
    ! -name setup-links.sh \
    ! -name \.             \
    ! -name .git           \
    ! -name mac_setup.rst  \
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
            printf "$skip: $source_name - already linked\n"

        else
            # echo "target_dir: '$existing_target_dir', dir: '$DIR'"
            printf "$error: $HOME/$source_name points to $existing_target\n"
        fi

    elif [[ -d "$target" ]]; then
        printf "$error: $target already exists as a directory\n"

    elif [[ -e "$target" ]]; then
        printf "$error: $target already exists as a file\n"

    elif ln -s "$DIR/$source_name" "$HOME"; then
        printf "$ok: $source_name - linked\n"

    else
        printf "$error: linking $source_name failed\n"
    fi

done
