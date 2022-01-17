#!/usr/bin/env bash

echo "-- Install additional package managers --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --noconfirm --needed fakeroot fwupd

cargo install topgrade cargo-update

if [[ ! -f "/usr/bin/paru" ]]; then
  work_dir=$(mktemp -d)

  trap 'rm -rf $work_dir' EXIT

  cd "$work_dir"

  curl -o paru-bin.tar.gz https://aur.archlinux.org/cgit/aur.git/snapshot/paru-bin.tar.gz
  tar xf paru-bin.tar.gz

  cd paru-bin
  makepkg -si --noconfirm
fi
