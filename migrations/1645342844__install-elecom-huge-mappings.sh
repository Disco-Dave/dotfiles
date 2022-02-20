#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ ! -f /etc/X11/xorg.conf.d/99-elecom-huge.conf ]]; then
  sudo ln -sf "$_DOTFILES_HOME/X11/99-elecom-huge.conf" /etc/X11/xorg.conf.d/99-elecom-huge.conf
fi
