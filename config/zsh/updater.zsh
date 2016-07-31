# Return 0 if not uncommited changes, return 1 otherwise.
function git-repo-is-clean() {
    git diff-index --quiet HEAD --exit-code
}

function update-dotfiles() {
    echo "$fg[white]Updating ~/.dotfiles $reset_color"
    cd ~/.dotfiles
    git-repo-is-clean
    local needsStash=$?
    if [[ "${needsStash}" -eq 1 ]]; then
        echo "$fg[white]Stashing changes.$reset_color"
        git stash
    else
        echo "$fg[white]No changes to stash.$reset_color"
    fi
    git pull origin master
    if [[ "${needsStash}" -eq 1 ]]; then
        echo "$fg[white]Popping stash.$reset_color"
        git stash pop
    fi
    cd -
}

# Update the vendor/st repo.  It's special because it's origin is my github repo
# which has personal tweaks on the 'tweaks' branch.
function update-dotfile-vendor-st() {
    pushd ~/.dotfiles/vendor/st
    git checkout master
    git pull upstream master
    git push origin master
    git checkout tweaks
    git rebase origin/master
    git push -f origin tweaks
    popd
}

# Update submodules in ~/.dotfiles/vendor and commit the changes.
function update-dotfile-vendors() {
    pushd ~/.dotfiles
    git submodule foreach git pull origin master
    update-dotfile-vendor-st
    git add vendor
    git commit -m "chore(git): update submodules"
    popd
}

function upgrade-dotfile-vendors() {
    
}

function update-dotfile-symlinks() {
    echo "$fg[white]Updating symlinks to ~/.dotfiles$reset_color"
    if [[ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]]; then
        rcup -t linux
    else
        rcup
    fi
}

function update-current-zsh() {
    echo "$fg[white]Updating current ZSH instance.$reset_color"
    reload-zshrc
}

function update-current-tmux() {
    if [[ -n "${TMUX}" ]]; then
        echo "$fg[white]Updating current Tmux instance.$reset_color"
        tmux source-file ~/.tmux.conf
			  tmux display-message -p "[From Tmux] Sourced .tmux.conf."
    fi
}

# Command to get this workstation synchronized.
function open-sesame() {
    echo "$fg[white]Welcome back! Lets get you up to speed...$reset_color"
    if which-command "open-sesame-system" > /dev/null; then
        open-sesame-system
        echo
    else
        echo "$fg[white]No system specific function found.$reset_color"
    fi
    update-dotfiles
    echo
    update-dotfile-symlinks
    echo
    update-current-zsh
    echo
    update-current-tmux
    echo "$fg[green]You're five-by-five, good-to-go.$reset_color"
}

