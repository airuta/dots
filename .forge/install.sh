#!/bin/sh

###############################################################################
# 0. Script settings.

CARGO_HOME="$HOME/.forge/cargo"
RUSTUP_HOME="$HOME/.forge/rustup"
OSTYPE=`uname`

if [ "$OSTYPE" == "Darwin" ]; then
        if [ -n "$BREW_LOCAL" ]; then
	    HOMEBREW_CASK_OPTS="--appdir=~/Applications --fontdir=~/Fonts"
        fi
fi

install() {
    if [ "$OSTYPE" == "Darwin" ]; then
        brew install $1
    fi
}

has_cmd() {
    command -v "$1" > /dev/null 2>&1
}

###############################################################################

echo
echo "STEP 1. Install git."
echo

if ! has_cmd "git"; then
    echo "Installing git..."
    install git
    echo "Running post-installation setup..."
    git config --global user.name "Valerii Praid"
    git config --global user.mail "valerii.praid@gmail.com"
    git config --global pull.ff only
    git config --global status.showUntrackedFiles normal
else
    echo "Skipping. Git is already installed."
fi

###############################################################################

echo
echo "STEP 2. Install rust and cargo."
echo

if ! has_cmd "rustup-init" || ! has_cmd "cargo"; then
    echo "Installing rustup-init..."
    install rustup-init
    echo "Installing rust..."
    rustup-init -y
    echo "Running post-installation setup..."
    source ~/.forge/cargo/env
    mkdir -p ~/.config/fish/completions
    rustup completions fish > ~/.config/fish/completions/rustup.fish
else
    echo "Skipping. Cargo and rust are already installed."
fi

###############################################################################

echo
echo "STEP 3. Install starship."
echo

if ! has_cmd "starship"; then
    cargo install starship
else
    echo "Skipping. Starship is already installed."
fi

###############################################################################

echo
echo "STEP 4. Install pyenv and python."
echo

if ! has_cmd "pyenv"; then
    echo "Installing pyenv..."
    install pyenv
    echo "Installing python..."
    pyenv install 3.8.11
    pyenv global 3.8.11
else
    echo "Skipping. Pyenv and python are already installed."
fi

###############################################################################

echo
echo "STEP 5. Install fish."
echo

if ! has_cmd "fish"; then
    echo "Installing fish shell..."
    install fish
    echo "Installing oh my fish..."
    curl -L https://get.oh-my.fish > install
    fish install --path=~/.forge/omf --config=~/.config/omf
    rm install
    echo "Applying a theme..."
    omf theme dracula
else
    echo "Skipping. Fish shell is already installed."
fi

fi

###############################################################################

# Install fish
# Install alacritty
# Install bat
# Install fonts
