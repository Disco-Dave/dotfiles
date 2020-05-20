#!/bin/bash

set -e

# Install neovim
sudo pacman -S --needed --noconfirm neovim nodejs npm python python2 ruby xclip

# Create npmrc
if [[ ! -f "$HOME/.config/npm/npmrc" ]]; then
    mkdir -p "$HOME/.config/npm"
    echo "prefix=$HOME/.local/share/npm" >> "$HOME/.config/npm/npmrc"
    echo "cache=$HOME/.cache/npm" >> "$HOME/.config/npm/npmrc"
    echo "tmp=\$XDG_RUNTIME_DIR/npm" >> "$HOME/.config/npm/npmrc"
    echo "init-module=$HOME/.config/npm/config/npm-init.js" >> "$HOME/.config/npm/npmrc"
fi

export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc"
npm install -g neovim

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
