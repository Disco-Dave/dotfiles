#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

mkdir -p "$XDG_CONFIG_HOME/pg"

rm -rf "$XDG_CONFIG_HOME/pg"
ln -sf "$_DOTFILES/pg" "$XDG_CONFIG_HOME/pg"
