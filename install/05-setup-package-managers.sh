#!/usr/bin/env bash

echo "-- Setup Package Managers --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --noconfirm --needed reflector fakeroot fwupd

sudo ln -sf "$_DOTFILES_HOME/pacman/reflector.conf" /etc/xdg/reflector/reflector.conf
sudo ln -sf "$_DOTFILES_HOME/pacman/pacman.conf" /etc/pacman.conf

if [ "$ENVIRONMENT" != "sandbox" ]; then
  sudo systemctl enable --now reflector.timer
fi

cargo install topgrade

if [[ ! -f "/usr/bin/paru" ]]; then
  work_dir=$(mktemp -d)

  trap 'rm -rf $work_dir' EXIT

  cd "$work_dir"

  curl -o paru-bin.tar.gz https://aur.archlinux.org/cgit/aur.git/snapshot/paru-bin.tar.gz
  tar xf paru-bin.tar.gz

  cd paru-bin
  makepkg -si --noconfirm
fi
