#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --noconfirm --needed npm

if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then

  mkdir -p "$XDG_CONFIG_HOME/npm"

  {
    echo "prefix=$XDG_DATA_HOME/npm"
    echo "cache=$XDG_CACHE_HOME/npm"
    echo "tmp=/tmp/npm"
    echo "init-module=$XDG_CONFIG_HOME/npm/config/npm-init.js" >> "$XDG_CONFIG_HOME/npm/npmrc"
  } >> "$XDG_CONFIG_HOME/npm/npmrc"

fi
