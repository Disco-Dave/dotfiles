#!/usr/bin/env zsh

echo "-- Setup Desktop --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

if [ "$ENVIRONMENT" != "sandbox" ]; then
  sudo pacman -S --needed --noconfirm \
    alacritty \
    arc-gtk-theme \
    arc-icon-theme \
    atril \
    blueman \
    bluez \
    bluez-utils \
    dmenu \
    engrampa \
    feh \
    firefox \
    gvfs \
    htop \
    libx11 \
    libxft \
    libxinerama \
    libxrandr \
    libxss \
    network-manager-applet \
    p7zip \
    pass \
    pasystray \
    pavucontrol \
    picom \
    pkgconf \
    pulseaudio \
    pulseaudio-alsa \
    pulseaudio-bluetooth \
    pulsemixer \
    redshift \
    stalonetray \
    thunar \
    ttf-hack \
    udisks2 \
    xclip \
    xcursor-vanilla-dmz \
    xdotool \
    xfce4-notifyd \
    xfce4-power-manager \
    xfce4-screenshooter \
    xorg

  # Enable bluetooth
  sudo systemctl enable --now bluetooth.service

  # icons
  ln -sf "$_DOTFILES_HOME/icons" "$HOME/.icons"

  # themes
  ln -sf "$_DOTFILES_HOME/gtk-2.0" "$XDG_CONFIG_HOME/gtk-2.0"
  ln -sf "$_DOTFILES_HOME/gtk-3.0" "$XDG_CONFIG_HOME/gtk-3.0"

  # X11
  ln -sf "$_DOTFILES_HOME/X11" "$XDG_CONFIG_HOME/X11"

  # xmonad
  ln -sf "$_DOTFILES_HOME/xmonad" "$XDG_CONFIG_HOME/xmonad"
  (
    cd "$XDG_CONFIG_HOME/xmonad"
    cabal install
    xmonad --recompile
  )

  # picom
  ln -sf "$_DOTFILES_HOME/picom" "$XDG_CONFIG_HOME/picom"

  # stalonetray
  ln -sf "$_DOTFILES_HOME/stalonetray" "$XDG_CONFIG_HOME/stalonetray"

  # alacritty
  ln -sf "$_DOTFILES_HOME/alacritty" "$XDG_CONFIG_HOME/alacritty"
  (
    cd "$XDG_CONFIG_HOME/alacritty"
    ./merge.sh
  )
fi
