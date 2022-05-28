#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

rm -rf "$XDG_CONFIG_HOME/pg"
ln -sf "$_DOTFILES_HOME/pg" "$XDG_CONFIG_HOME/pg"
