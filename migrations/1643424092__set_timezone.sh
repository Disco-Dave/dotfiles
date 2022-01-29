#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [ "$ENVIRONMENT" != "sandbox" ]; then
  sudo timedatectl set-timezone US/Eastern
  sudo timedatectl set-ntp true
fi
