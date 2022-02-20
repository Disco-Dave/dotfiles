#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --noconfirm --needed git
ln -sf "$_DOTFILES_HOME/git" "$HOME/.config/git"
