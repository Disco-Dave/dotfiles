#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ ! -f /usr/local/bin/set_elecom_huge_settings ]]; then
  sudo ln -sf "$_DOTFILES_HOME/scripts/set_elecom_huge_settings" /usr/local/bin/set_elecom_huge_settings
fi

if [[ ! -f /etc/udev/rules.d/99-elecom-huge-trackball.rules ]]; then
  sudo ln -sf "$_DOTFILES_HOME/udev/99-elecom-huge-trackball.rules" /etc/udev/rules.d/99-elecom-huge-trackball.rules
fi
