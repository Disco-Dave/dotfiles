#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

rm -rf "$HOME/.inputrc"
rm -rf "$HOME_CONFIG_HOME/readline"
ln -sf "$_DOTFILES_HOME/readline" "$XDG_CONFIG_HOME/readline"
