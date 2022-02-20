#!/bin/zsh

set -e

source "$_DOTFILES_HOME/zsh/zshenv"

sudo pacman -S --noconfirm --needed rustup openssl pkgconf

rustup toolchain install stable

cargo install cargo-watch cargo-tarpaulin cargo-edit
