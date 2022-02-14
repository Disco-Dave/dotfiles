#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"


if [[ "$_HOSTNAME" != "virt" && "$_HOSTNAME" != "sandbox" ]]; then
  sudo pacman -S --needed docker

  sudo gpasswd -a david docker

  sudo systemctl enable --now docker
fi
