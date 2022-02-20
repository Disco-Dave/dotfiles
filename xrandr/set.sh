#!/usr/bin/env bash

CURRENT_HOST="$(hostnamectl hostname)"
XRANDR_PATH="$(realpath "$0" | xargs dirname)"
TARGET_SCRIPT="$XRANDR_PATH/$CURRENT_HOST.sh"

if [[ -x "$TARGET_SCRIPT" ]]; then
  $TARGET_SCRIPT
fi
