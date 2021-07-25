if status is-interactive
    # Forge locations
    set -gx CARGO_HOME ~/.forge/cargo
    set -gx GOPATH ~/.forge/go
    set -gx PYENV_ROOT $HOME/.forge/pyenv
    # Path
    fish_add_path $CARGO_HOME/bin
    fish_add_path $GOPATH/bin
    fish_add_path $PYENV_ROOT/bin
end

# Dotfiles
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
dots config --local status.showUntrackedFiles no

# Set up starship
starship init fish | source

# Set up pyenv
status is-interactive; and pyenv init --path | source
pyenv init - | source
