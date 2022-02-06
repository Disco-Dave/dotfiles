#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --noconfirm --needed reflector

sudo ln -sf "$_DOTFILES_HOME/pacman/reflector.conf" /etc/xdg/reflector/reflector.conf
sudo ln -sf "$_DOTFILES_HOME/pacman/pacman.conf" /etc/pacman.conf

if [ "$_HOSTNAME" != "sandbox" ]; then
  sudo systemctl enable reflector.timer
fi

sed '/^#.*/d' /etc/xdg/reflector/reflector.conf | xargs sudo reflector
sudo pacman -Syu --noconfirm archlinux-keyring
