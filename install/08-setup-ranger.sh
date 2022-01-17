#!/usr/bin/env bash

echo "-- Setup ranger --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --needed --noconfirm ranger

ln -sfn "$_DOTFILES_HOME/ranger" "$XDG_CONFIG_HOME/ranger"
