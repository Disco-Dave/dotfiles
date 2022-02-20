#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ "$_HOSTNAME" != "sandbox" ]]; then
  sudo pacman -S --needed --noconfirm miniserve

  mkdir -p ~/.local/share/sources
  cd ~/.local/share/sources

  git clone https://github.com/Disco-Dave/homepage-server.git
  cd homepage-server

  stack install

  if [[ ! -e "$XDG_CONFIG_HOME/homepage-server" ]]; then
    ln -sf "$_DOTFILES_HOME/homepage-server/config" "$XDG_CONFIG_HOME/homepage-server"
  fi

  if [[ ! -e "$XDG_DATA_HOME/homepage-server" ]]; then
    ln -sf "$_DOTFILES_HOME/homepage-server/data" "$XDG_DATA_HOME/homepage-server"
  fi

  if [[ ! -e "$XDG_CONFIG_HOME/systemd/user/homepage-server.service" ]]; then
    ln -sf "$_DOTFILES_HOME/homepage-server/homepage-server.service" "$XDG_CONFIG_HOME/systemd/user/homepage-server.service"
  fi

  systemctl --user enable --now homepage-server
fi
