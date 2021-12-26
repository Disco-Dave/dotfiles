#!/usr/bin/env bash

set -ex

export _DOTFILES_HOME="${DOTFILES_HOME:-$HOME/.config/dotfiles}"

# Install required packages needed to get started
sudo pacman -S --noconfirm --needed git zsh

# Clone dotfiles repo
if [ ! -d $_DOTFILES_HOME ]; then
  git clone git@github.com:Disco-Dave/dotfiles.git $_DOTFILES_HOME
fi

run-parts --regex '.*' --exit-on-error --verbose -- "$_DOTFILES_HOME/install"
