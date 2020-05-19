#!/bin/bash

set -e

# Install git
sudo pacman -S --needed --noconfirm git

# Use XDG config directory for git
mkdir -p "$HOME/.config"
ln -sf "$(pwd)" "$HOME/.config/git"
