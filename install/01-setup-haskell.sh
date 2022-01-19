#!/usr/bin/env zsh

echo "-- Setup Haskell --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

# Install dependencies for ghcup, ghc, cabal, stack, and hls
sudo pacman -S --noconfirm --needed \
  automake \
  curl \
  gcc \
  git \
  gmp \
  gnupg \
  icu \
  libffi \
  make \
  ncurses \
  perl \
  tar \
  xz \
  zlib

# Install ghcup, ghc, cabal, stack, and hls
if ! command -v ghcup &> /dev/null; then
  export BOOTSTRAP_HASKELL_NONINTERACTIVE="yes"
  export BOOTSTRAP_HASKELL_INSTALL_STACK="yes"
  export BOOTSTRAP_HASKELL_INSTALL_HLS="yes"

  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
fi

# Set ghci settings
ln -sfn "$_DOTFILES_HOME/haskell/ghci" "$HOME/.ghci"
ln -sfn "$_DOTFILES_HOME/haskell/haskeline" "$HOME/.haskeline"

# Set stack settings
mkdir -p "$STACK_ROOT"
ln -sfn "$_DOTFILES_HOME/haskell/stack-config.yaml" "$STACK_ROOT/config.yaml"

# Set fourmolu settings
ln -sfn "$_DOTFILES_HOME/haskell/fourmolu.yaml" "$XDG_CONFIG_HOME/fourmolu.yaml"
