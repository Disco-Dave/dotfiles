#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

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
    firefox-tridactyl \
    gvfs \
    htop \
    libx11 \
    libxft \
    libxinerama \
    libxrandr \
    libxss \
    mpc \
    mpd \
    mpv \
    ncmpcpp \
    network-manager-applet \
    p7zip \
    pass \
    passff-host \
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
    xdg-user-dirs \
    xdotool \
    xfce4-notifyd \
    xfce4-power-manager \
    xfce4-screenshooter \
    xorg \
    xorg-xinit \
    xorg-xmessage

  # Generate xdg user directories
  xdg-user-dirs-update

  # Enable bluetooth
  sudo systemctl enable --now bluetooth.service

  # Enable pulseaudio
  systemctl --user enable --now pulseaudio

  # icons
  if [[ ! -e "$_DOTFILES_HOME/icons" ]]; then
    ln -sf "$_DOTFILES_HOME/icons" "$HOME/.icons"
  fi

  # themes
  if [[ ! -e "$_DOTFILES_HOME/gtk-2.0" ]]; then
    ln -sf "$_DOTFILES_HOME/gtk-2.0" "$XDG_CONFIG_HOME/gtk-2.0"
  fi
  if [[ ! -e "$_DOTFILES_HOME/gtk-3.0" ]]; then
    ln -sf "$_DOTFILES_HOME/gtk-3.0" "$XDG_CONFIG_HOME/gtk-3.0"
  fi

  # X11
  if [[ ! -e "$_DOTFILES_HOME/X11" ]]; then
    ln -sf "$_DOTFILES_HOME/X11" "$XDG_CONFIG_HOME/X11"
  fi

  # xmobar
  if [[ ! -e "$_DOTFILES_HOME/xmobar" ]]; then
    ln -sf "$_DOTFILES_HOME/xmobar" "$XDG_CONFIG_HOME/xmobar"
  fi
  (
    cd "$XDG_CONFIG_HOME/xmobar"
    cabal install --overwrite-policy=always
  )

  # xmonad
  if [[ ! -e "$_DOTFILES_HOME/xmonad" ]]; then
    ln -sf "$_DOTFILES_HOME/xmonad" "$XDG_CONFIG_HOME/xmonad"
  fi
  (
    cd "$XDG_CONFIG_HOME/xmonad"
    cabal install --overwrite-policy=always
    xmonad --recompile
  )

  # picom
  if [[ ! -e "$_DOTFILES_HOME/picom/picom.conf" ]]; then
    ln -sf "$_DOTFILES_HOME/picom/picom.conf" "$XDG_CONFIG_HOME/picom/picom.conf"
  fi

  # stalonetray
  if [[ ! -e "$_DOTFILES_HOME/stalonetray" ]]; then
    ln -sf "$_DOTFILES_HOME/stalonetray" "$XDG_CONFIG_HOME/stalonetray"
  fi

  # alacritty
  if [[ ! -e "$_DOTFILES_HOME/alacritty" ]]; then
    ln -sf "$_DOTFILES_HOME/alacritty" "$XDG_CONFIG_HOME/alacritty"
  fi
  (
    cd "$XDG_CONFIG_HOME/alacritty"
    ./merge.sh
  )

  # xrandr
  if [[ ! -e "$_DOTFILES_HOME/xrandr" ]]; then
    ln -sf "$_DOTFILES_HOME/xrandr" "$XDG_CONFIG_HOME/xrandr"
  fi

  # TODO NEEDS FIXED!
  # mpd
  if [[ ! -e "$_DOTFILES_HOME/mpd" ]]; then
    ln -sf "$_DOTFILES_HOME/mpd" "$XDG_CONFIG_HOME/mpd"
  fi
  mkdir -p "$XDG_DATA_HOME/mpd/playlists"
  mkdir -p "$XDG_CACHE_HOME/mpd"
  systemctl --user enable --now mpd
  mpc update

  # ncmpcpp
  if [[ ! -e "$_DOTFILES_HOME/ncmpcpp" ]]; then
    ln -sf "$_DOTFILES_HOME/ncmpcpp" "$XDG_CONFIG_HOME/ncmpcpp"
  fi
fi
