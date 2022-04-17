#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ -e ~/.config/topgrade.toml ]]; then
  rm ~/.config/topgrade.toml
fi

ln -sf "$_DOTFILES_HOME/topgrade/topgrade.toml" "$HOME/.config/topgrade.toml"
