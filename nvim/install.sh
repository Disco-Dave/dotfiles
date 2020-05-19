#!/bin/bash

set -e

# Install neovim
sudo pacman -S --needed --noconfirm neovim nodejs npm python python2 ruby xclip

# Create npmrc
if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then
    mkdir -p "$XDG_CONFIG_HOME/npm"
    echo "prefix=${XDG_DATA_HOME}/npm" >> "$XDG_CONFIG_HOME/npm"
    echo "cache=${XDG_CACHE_HOME}/npm" >> "$XDG_CONFIG_HOME/npm"
    echo "tmp=${XDG_RUNTIME_DIR}/npm" >> "$XDG_CONFIG_HOME/npm"
    echo "init-module=${XDG_CONFIG_HOME}/npm/config/npm-init.js" >> "$XDG_CONFIG_HOME/npm"
fi

# Create directories
ln -sf "$(pwd)" "$HOME/.config/nvim"
mkdir -p "$HOME/.local/share/nvim/backup"
mkdir -p "$HOME/.local/share/nvim/swap"
mkdir -p "$HOME/.local/share/nvim/undo"
mkdir -p "$HOME/.local/share/nvim/language-servers"

# Install vim-plug
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

# Install all plugins via vim-plug
nvim +PlugInstall +qall
