#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ ! -f /etc/X11/xorg.conf.d/99-sanwa-gravi.conf ]]; then
  sudo ln -sf "$_DOTFILES_HOME/X11/99-sanwa-gravi.conf" /etc/X11/xorg.conf.d/99-sanwa-gravi.conf
fi

if [[ ! -f /etc/X11/xorg.conf.d/99-ploopy-classic.conf ]]; then
  sudo ln -sf "$_DOTFILES_HOME/X11/99-ploopy-classic.conf" /etc/X11/xorg.conf.d/99-ploopy-classic.conf
fi
