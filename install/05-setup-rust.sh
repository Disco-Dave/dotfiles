#!/usr/bin/env zsh

echo "-- Setup Rust --"

source "$_DOTFILES_HOME/zsh/zshenv"
set -e

sudo pacman -S --noconfirm --needed rustup openssl pkgconf

rustup toolchain install stable

cargo install cargo-watch cargo-tarpaulin cargo-edit
