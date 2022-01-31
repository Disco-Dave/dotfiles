#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ "$(hostnamectl hostname)" == "laptop" ]]; then
  paru -S --needed --noconfirm auto-cpufreq

  sudo rm -f /etc/auto-cpufreq.conf
  sudo ln -sf "$_DOTFILES_HOME/auto-cpufreq/auto-cpufreq.conf" /etc/auto-cpufreq.conf

  sudo systemctl enable --now auto-cpufreq
fi
