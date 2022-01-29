#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

mkdir -p \
  "$XDG_CONFIG_HOME" \
  "$XDG_CACHE_HOME" \
  "$XDG_DATA_HOME" \
  "$XDG_BIN_HOME" \
  "$XDG_STATE_HOME"
