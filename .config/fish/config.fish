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
