#!/usr/bin/env bash

set -e

export _DOTFILES_HOME="${DOTFILES_HOME:-$HOME/.config/dotfiles}"

mkdir -p \
  "$HOME/.config" \
  "$HOME/.cache" \
  "$HOME/.local/share" \
  "$HOME/.local/bin" \
  "$HOME/.local/state"


sudo timedatectl set-timezone US/Eastern
sudo timedatectl set-ntp true

# Install required packages needed to get started
#sudo curl https://archlinux.org/mirrorlist/all/ --output /etc/pacman.d/mirrorlist
sudo pacman -Syu --noconfirm
sudo pacman -S --noconfirm --needed git openssh run-parts zsh

# Clone dotfiles repo
if [ ! -d "$_DOTFILES_HOME" ]; then
  git clone git@github.com:Disco-Dave/dotfiles.git "$_DOTFILES_HOME"
fi

ln -sf "$_DOTFILES_HOME/git" "$HOME/.config/git"
ln -sf "$_DOTFILES_HOME/scripts" "$HOME/.local/scripts"

run-parts --regex '.*' --exit-on-error --verbose -- "$_DOTFILES_HOME/install"
