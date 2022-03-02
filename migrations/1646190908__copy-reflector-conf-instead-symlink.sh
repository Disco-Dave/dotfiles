#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo rm /etc/xdg/reflector/reflector.conf

sudo cp "$_DOTFILES_HOME/pacman/reflector.conf" /etc/xdg/reflector/reflector.conf
