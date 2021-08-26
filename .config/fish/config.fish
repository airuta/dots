if status is-interactive
    fish_vi_key_bindings
end

# Dotfiles
alias dots='/usr/bin/git --git-dir=$HOME/.dots/ --work-tree=$HOME'
dots config --local status.showUntrackedFiles no

# Set up starship
starship init fish | source

# Set up pyenv
status is-interactive; and pyenv init --path | source
pyenv init - | source
status --is-interactive; and pyenv virtualenv-init - | source

# Set up ghcup
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
fish_add_path $HOME/.cabal/bin
fish_add_path $HOME/.ghcup/bin
