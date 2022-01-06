#!/usr/bin/env zsh

echo "-- Setup NPM --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e


sudo pacman -S --noconfirm --needed npm

if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then
    mkdir -p "$XDG_CONFIG_HOME/npm"
    echo "prefix=$XDG_DATA_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "cache=$XDG_CACHE_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "tmp=\$XDG_RUNTIME_DIR/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "init-module=$XDG_CONFIG_HOME/npm/config/npm-init.js" >> "$XDG_CONFIG_HOME/npm/npmrc"
fi

