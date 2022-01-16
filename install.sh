#!/usr/bin/env bash

set -e

export _DOTFILES_HOME="${DOTFILES_HOME:-$HOME/.config/dotfiles}"

mkdir -p \
  "$HOME/.config" \
  "$HOME/.cache" \
  "$HOME/.local/share" \
  "$HOME/.local/bin" \
  "$HOME/.local/state"

# Install required packages needed to get started
sudo pacman -S --noconfirm --needed git openssh run-parts

# Clone dotfiles repo
if [ ! -d "$_DOTFILES_HOME" ]; then
  git clone git@github.com:Disco-Dave/dotfiles.git "$_DOTFILES_HOME"
fi

ln -sf "$_DOTFILES_HOME/git" "$HOME/.config/git"

run-parts --regex '.*' --exit-on-error --verbose -- "$_DOTFILES_HOME/install"
