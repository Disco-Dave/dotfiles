#!/bin/bash

export STACK_ROOT="$HOME/.local/share/stack"

sudo pacman -S --needed --noconfirm gcc make zlib gmp

curl -sSL https://get.haskellstack.org/ | sh
stack setup
