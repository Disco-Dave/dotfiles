#!/usr/bin/env zsh

echo "-- Configure pacman  --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --noconfirm --needed reflector

sudo ln -sf "$_DOTFILES_HOME/pacman/reflector.conf" /etc/xdg/reflector/reflector.conf
sudo ln -sf "$_DOTFILES_HOME/pacman/pacman.conf" /etc/pacman.conf

if [ "$ENVIRONMENT" != "sandbox" ]; then
  sudo systemctl enable reflector.timer
fi

sed '/^#.*/d' /etc/xdg/reflector/reflector.conf | xargs sudo reflector
sudo pacman -Sy
