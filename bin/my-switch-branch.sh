#!/bin/bash

# Given a directory and repository branch, switch to that branch, stashing
# changes.

# Return 0 if not uncommited changes, return 1 otherwise.
function git-repo-is-clean() {
    git diff-index --quiet HEAD --exit-code
}

# Given a file path, update the git repository at that path from origin/master.
# If the repository has local changes, stash the changes and pop them after
# git-pull completes.
function change-git-branch() {
    echo repoDir "${repoDir}"
    echo repoDir "${repoBranch}"
    if [[ ! -e "${repoDir}" ]]; then
        return 1
    fi
    pushd "${repoDir}" > /dev/null
    git-repo-is-clean
    # Get the exit code.
    local needsStash=$?
    if [[ "${needsStash}" -eq 1 ]]; then
        git stash --quiet
    fi

    git checkout "${repoBranch}"

    popd > /dev/null
}

change-git-branch "$@"
