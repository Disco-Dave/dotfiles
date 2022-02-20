#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --needed --noconfirm \
  avahi \
  cups \
  cups-filters \
  foomatic-db  \
  foomatic-db-gutenprint-ppds \
  foomatic-db-nonfree \
  foomatic-db-nonfree-ppds \
  foomatic-db-ppds \
  ghostscript \
  gutenprint \
  hplip \
  sane-airscan \
  simple-scan \
  system-config-printer

paru -S --needed --noconfirm epson-inkjet-printer-escpr

if [[ "$_HOSTNAME" != "sandbox" ]]; then
  sudo systemctl enable --now cups
fi
