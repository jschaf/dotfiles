#/bin/zsh

DOTFILES_DIR=${DOTFILES_DIR:-"${HOME}/.dotfiles"}
DOTFILES_VENDOR_DIR="${DOTFILES_DIR}/vendor"

function updater-print-info() {
    local message="$1"
    echo "$fg[white]${message}$reset_color"
}

function updater-print-success() {
    local message="$1"
    echo "$fg[green]${message}$reset_color"
}

# Return 0 if not uncommited changes, return 1 otherwise.
function git-repo-is-clean() {
    git diff-index --quiet HEAD --exit-code
}

function update-dotfiles() {
    updater-print-info "Updating ~/.dotfiles "
    pushd "${DOTFILES_DIR}"
    git-repo-is-clean
    local needsStash=$?
    if [[ "${needsStash}" -eq 1 ]]; then
        updater-print-info "Stashing changes."
        git stash
    else
        updater-print-info "No changes to stash."
    fi
    git pull origin master
    if [[ "${needsStash}" -eq 1 ]]; then
        updater-print-info "Popping stash."
        git stash pop
    fi
    popd
}

# Update the vendor/st repo.  It's special because it's origin is my github repo
# which has personal tweaks on the 'tweaks' branch.
function update-dotfile-vendor-st() {
    pushd "${DOTFILES_VENDOR_DIR}/st"
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
    pushd "${DOTFILES_DIR}"
    git submodule foreach git pull origin master
    update-dotfile-vendor-st
    git add vendor
    git commit -m "chore(git): update submodules"
    popd
}

function upgrade-dotfile-vendors() {
    
}

function update-dotfile-symlinks() {
    updater-print-info "Updating symlinks to ~/.dotfiles"
    if [[ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]]; then
        rcup -t linux
    else
        rcup
    fi
}

function update-current-zsh() {
    updater-print-info "Updating current ZSH instance."
    reload-zshrc
}

function update-current-tmux() {
    if [[ -n "${TMUX}" ]]; then
        updater-print-info "Updating current Tmux instance."
        tmux source-file ~/.tmux.conf
			  tmux display-message -p "[From Tmux] Sourced .tmux.conf."
    fi
}

# Command to get this workstation synchronized with the latest changes dotfile
# changes.
function open-sesame() {
    updater-print-info "Welcome back! Lets get you up to speed..."
    if which-command "open-sesame-system" > /dev/null; then
        open-sesame-system
        echo
    else
        updater-print-info "No system specific function found."
    fi
    update-dotfiles
    echo
    update-dotfile-symlinks
    echo
    update-current-zsh
    echo
    update-current-tmux
    updater-print-success "  You're five-by-five, good-to-go.  "
}

