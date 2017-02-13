#/bin/zsh

DOTFILES_DIR=${DOTFILES_DIR:-"${HOME}/.dotfiles"}
DOTFILES_VENDOR_DIR="${DOTFILES_DIR}/vendor"
TPM_HOME="${HOME}/.tmux/plugins/tpm"

# Return 0 if not uncommited changes, return 1 otherwise.
function git-repo-is-clean() {
  git diff-index --quiet HEAD --exit-code
}

function updater-pushd() {
  pushd "$@" > /dev/null
}


function updater-popd() {
  popd "$@" > /dev/null
}

function ensure-tmux-package-manager-is-installed() {
    if [[ ! -d "${TPM_HOME}" ]]; then
        print-error "tmux package manager (TPM) is not installed at ${TPM_HOME}"
    fi
}

function ensure-insync-is-running() {
  if ! command -v insync > /dev/null; then
    return
  fi

  print-info "Ensuring org files are up-to-date."

  if pgrep "insync" > /dev/null; then
    print-success "insync is running."
  else
    $(insync start)
    print-success "Started insync."
  fi
}

# Given a file path, update the git repository at that path from origin/master.
# If the repository has local changes, stash the changes and pop them after
# git-pull completes.
function update-git-repo() {
  local repoDir="${1}"
  print-info "Updating git repository at ${repoDir}."
  if [[ ! -e "${repoDir}" ]]; then
    return
    print-error "Can't find repository at ${repoDir}."
  fi
  updater-pushd "${repoDir}"
  git-repo-is-clean
  # Get the exit code.
  local needsStash=$?
  if [[ "${needsStash}" -eq 1 ]]; then
    git stash --quiet
  fi

  if git pull origin master --quiet; then
    print-success "Git pulled changes succesfully."
  else
    print-error "Unable to pull changes."
  fi

  if [[ "${needsStash}" -eq 1 ]]; then
    git stash pop --quiet
  fi
  updater-popd
}

function update-googlejs-repo() {
  update-git-repo "${HOME}/prog/googlejs"
}

function update-dotfile-repo() {
  update-git-repo "${DOTFILES_DIR}"
}

# Update the vendor/st repo.  It's special because it's origin is my github repo
# which has personal tweaks on the 'tweaks' branch.
function update-dotfile-vendors-st() {
  updater-pushd "${DOTFILES_VENDOR_DIR}/st"
  git checkout master
  git pull upstream master
  git push origin master
  git checkout tweaks
  git rebase origin/master
  git push -f origin tweaks
  updater-popd
}

# Update submodules in ~/.dotfiles/vendor and commit the changes.
function update-dotfile-vendors() {
  updater-pushd "${DOTFILES_DIR}"
  git submodule update --init
  git submodule foreach git pull origin master
  update-dotfile-vendors-st
  git add vendor
  git commit -m "chore(git): update submodules"
  updater-popd
}

# Install the current state of the st repository.
function upgrade-dotfile-vendors-st() {
  if [[ $(uname -s) = "Linux" ]]; then
    updater-pushd "${DOTFILES_VENDOR_DIR}/st"
    print-info "Upgrading suckless terminal."
    # If we don't remove config.h, then changes in config.def.h are not
    # generated to replace config.h.  config.h is not tracked by git and is
    # generated from config.def.h so we always want to replace it.
    rm -f config.h
    sudo make clean install
    updater-popd
  else
    print-info "Skipping st on this platform."
  fi
}

function upgrade-dotfile-vendors() {
  updater-pushd "${DOTFILES_DIR}"
  # zgen is sourced by zshrc.

  # PathPicker - we only need to make sure a symlink exists.  -L means file exists
  # and is a symlink.
  if [[ ! -L "${HOME}/bin/fpp" ]]; then
    print-info "Upgrading PathPicker."
    ln -s "${DOTFILES_VENDOR_DIR}/PathPicker/fpp" "${HOME}/bin"
  else
    print-info "PathPicker is up to date."
  fi

  # fzf
  echo "Installing FZF binary."
  print-info "Upgrading fzf."
  "${DOTFILES_VENDOR_DIR}/fzf/install" --bin --no-update-rc --no-key-bindings --no-completion

  # nvm is sourced by .zshrc.

  upgrade-dotfile-vendors-st

  updater-popd
}

function update-dotfile-symlinks() {
  print-info "Updating symlinks to ~/.dotfiles"
  name="$(uname -s)"
  if [[ "${name[1,5]}" == 'Linux' ]]; then
    rcup -t linux
  else
    rcup
  fi
  print-success "Symlinks updated."
}

function update-current-zsh() {
  print-info "Updating current ZSH instance."
  if reload-zshrc; then
    print-success "This ZSH instance was updated."
  else
    print-error "This ZSH instance was not updated."
  fi
}

function update-current-tmux() {
  if [[ -n "${TMUX}" ]]; then
    print-info "Updating current Tmux instance."
    local tmuxSourceOutput="$(tmux source-file ~/.tmux.conf)"
    if [[ -z "${tmuxSourceOutput}" ]]; then
      print-success "tmux reloaded."
    else
      print-error "tmux didn't reload correctly.  Is there a parse error?"
    fi
  fi
}

function update-emacs-buffers() {
  print-info "Reverting Emacs buffers."
  if emacsclient --no-wait --alternate-editor=false \
    --quiet --eval '(revbufs)'; then
    print-success "Emacs buffers updated."
  else
    print-error "emacs didn't revert buffers.  Is emacs started?"
  fi
}

function update-gdrive-git-repo() {
  update-git-repo "${HOME}/gdrive"
}

# Command to get this workstation synchronized with the latest changes dotfile
# changes.
function open-sesame() {
  print-info "Welcome back! Lets get you up to speed..."
  echo
  if which-command "open-sesame-system" > /dev/null; then
    open-sesame-system
    echo
  else
    print-info "No system specific open-sesame function found."
    echo
  fi
  update-dotfile-repo
  echo
  update-gdrive-git-repo
  echo
  update-googlejs-repo
  echo
  update-emacs-buffers
  echo
  update-dotfile-symlinks
  echo
  update-current-zsh
  echo
  update-current-tmux
  echo
  echo
  print-success "You're five-by-five, good-to-go.  "
}

# Create a patched consolas font.  The nerd-fonts repo should be on my
# 'consolas' branch
function make-consolas-nerd-font() {
  print-info "Creating consolas nerd font."
  updater-pushd "${HOME}/prog/nerd-fonts"
  ./font-patcher --quiet --fontawesome --fontlinux --octicons --pomicons \
    --powerline --powerlineextra \
    --outputdir "${DOTFILES_DIR}/fonts" \
    "${DOTFILES_DIR}/fonts/consola.ttf"
  "${DOTFILES_DIR}/fonts/install.sh"
  st zsh -c '. ~/.zshrc; test-fonts; $SHELL'
  updater-popd
}
