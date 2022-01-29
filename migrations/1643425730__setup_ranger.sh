#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm ranger
ln -sfn "$_DOTFILES_HOME/ranger" "$XDG_CONFIG_HOME/ranger"
