#!/usr/bin/env zsh

source "$_DOTFILES_HOME/zsh/zshenv"
set -ex

mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" "$XDG_BIN_HOME"
