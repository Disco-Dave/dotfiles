#!/bin/bash

set -e

if [[ ! -f "/bin/git" ]]; then
    sudo pacman -S --noconfirm git
fi

if [[ ! -d "$HOME/.config/dotfiles" ]]; then
    mkdir -p "$HOME/.config"
    git clone https://github.com/Disco-Dave/dotfiles.git "$HOME/.config/dotfiles"

    (cd "$HOME/.config/dotfiles/git"; ./install.sh)
    (cd "$HOME/.config/dotfiles/zsh"; ./install.sh)
    (cd "$HOME/.config/dotfiles/nvim"; ./install.sh)
fi
