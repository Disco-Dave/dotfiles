#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm tmux

ln -sfn "$_DOTFILES_HOME/tmux" "$XDG_CONFIG_HOME/tmux"
