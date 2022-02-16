#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

if [[ "$_HOSTNAME" != "virt" && "$_HOSTNAME" != "sandbox" ]]; then
  sudo pacman -S --needed \
    bridge-utils \
    dmidecode \
    dnsmasq \
    edk2-ovmf \
    iptables-nft \
    libvirt \
    openbsd-netcat \
    qemu \
    virt-manager

  sudo gpasswd -a david libvirt

  sudo systemctl enable --now libvirtd
  sudo systemctl enable --now virtlogd

  sudo virsh net-autostart default 
fi
