# Global paths
set -gx XDG_DATA_HOME $HOME/.data
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_STATE_HOME $HOME/.state

# Rust settings
set -gx CARGO_HOME $XDG_DATA_HOME/cargo
set -gx RUSTUP_HOME $XDG_DATA_HOME/rustup
fish_add_path $CARGO_HOME/bin

# Golang settings
set -xg GOPATH $XDG_DATA_HOME/go
fish_add_path $GOPATH/bin

# Python settings
set -gx PYENV_ROOT $XDG_DATA_HOME/pyenv
fish_add_path $PYENV_ROOT/bin
set -xg PYTHONDONTWRITEBYTECODE 1

# Other stuff
set -gx DOOMDIR $XDG_CONFIG_HOME/doom
set -gx HOMEBREW_CASK_OPTS --appdir=~/Applications --fontdir=~/Fonts

# LLVM
set -gx LDFLAGS "-L/Users/valeriip/homebrew/opt/llvm/lib"
set -gx CPPFLAGS "-I/Users/valeriip/homebrew/opt/llvm/include"
