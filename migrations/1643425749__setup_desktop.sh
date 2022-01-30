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

  # xrandr
  ln -sf "$_DOTFILES_HOME/xrandr" "$XDG_CONFIG_HOME/xrandr"

  # mpd
  ln -sf "$_DOTFILES_HOME/mpd" "$XDG_CONFIG_HOME/mpd"
  mkdir -p "$XDG_DATA_HOME/mpd/playlists"
  systemctl --user enable mpd
  mpc update

  # ncmpcpp
  ln -sf "$_DOTFILES_HOME/ncmpcpp" "$XDG_CONFIG_HOME/ncmpcpp"
fi
