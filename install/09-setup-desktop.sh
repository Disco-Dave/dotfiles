#!/usr/bin/env zsh

echo "-- Setup Desktop --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

# TODO Audio including bluetooth

# xmonad
sudo pacman -S --needed --noconfirm git xorg libx11 libxft libxinerama libxrandr libxss pkgconf

ln -sf "$_DOTFILES_HOME/xmonad" "$XDG_CONFIG_HOME/xmonad"
(
  cd "$XDG_CONFIG_HOME/xmonad"
  cabal install
)

# TODO xmobar
# TODO picom
# TODO stalonetray
# TODO alacritty
# TODO gtk
# TODO icons
