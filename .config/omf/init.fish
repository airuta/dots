# Rust settings
set -gx CARGO_HOME $HOME/.forge/cargo
set -gx RUSTUP_HOME $HOME/.forge/rustup
fish_add_path $CARGO_HOME/bin

# Golang settings
set -xg GOPATH $HOME/.forge/go
fish_add_path $GOPATH/bin

# Python settings
set -gx PYENV_ROOT $HOME/.forge/pyenv
fish_add_path $PYENV_ROOT/bin
set -xg PYTHONDONTWRITEBYTECODE 1
