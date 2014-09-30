#!/bin/sh
setup_dotfiles() {
    local DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

    RESTORE='\033[0m'
    RED='\033[00;31m'
    GREEN='\033[00;32m'
    YELLOW='\033[00;33m'
    BLUE='\033[00;34m'

    ok="${GREEN}    ok${RESTORE}"
    skip="  skip"
    warn="${YELLOW}  warn${RESTORE}"
    error="${RED} error${RESTORE}"

    linkees=$(find "$DIR" -maxdepth 1 \
        ! -name \.             \
        ! -name shortcuts.sh   \
        ! -name mac_setup.rst  \
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
                printf "$skip: $source_name - already linked\n"

            else
                # echo "target_dir: '$existing_target_dir', dir: '$DIR'"
                printf "$error: $HOME/$source_name points to $existing_target\n"
            fi

        elif [[ -d "$target" ]]; then
            printf "$error: $target already exists as a directory\n"

        elif [[ -e "$target" ]]; then
            if [[ $OSTYPE == 'msys' ]]; then
                cp "$DIR/$source_name" "$HOME"
                printf "$warn: $source_name overwritten\n"
            else
                printf "$error: $target already exists as a file\n"
            fi
        elif ln -s "$DIR/$source_name" "$HOME"; then
            printf "$ok: $source_name - linked\n"

        else
            printf "$error: linking $source_name failed\n"
        fi

    done
}

create_project() {
    local repo_name="$1"
    local project_root="$HOME/prog"
    echo "Creating project at $project_root/$repo_name"
    local project_abs_path="$project_root/$repo_name"
    mkdir "$project_abs_path"
    cd "$project_abs_path"
    git init
    touch README.md
    git add README.md
    git commit -m "initial commit"
    echo -ne '\n' | github_create
}

github_create() {
  repo_name=$1

  dir_name=`basename $(pwd)`
  invalid_credentials=0

  if [ "$repo_name" = "" ]; then
    echo "  Repo name (hit enter to use '$dir_name')?"
    read repo_name
  fi

  if [ "$repo_name" = "" ]; then
    repo_name=$dir_name
  fi

  username=`git config github.user`
  if [ "$username" = "" ]; then
    echo "  Could not find username, run 'git config --global github.user <username>'"
    invalid_credentials=1
  fi

  token=`git config github.token`
  if [ "$token" = "" ]; then
    echo "  Could not find token, run 'git config --global github.token <token>'"
    invalid_credentials=1
  fi

  if [ "$invalid_credentials" -eq "1" ]; then
    return 1
  fi

  echo -n "  Creating Github repository '$repo_name' ..."
  curl -u "$username:$token" https://api.github.com/user/repos -d '{"name":"'$repo_name'"}' > /dev/null 2>&1
  echo " done."

  echo -n "  Pushing local code to remote ..."
  git remote add origin git@github.com:$username/$repo_name.git > /dev/null 2>&1
  git push -u origin master > /dev/null 2>&1
  echo " done."
}
