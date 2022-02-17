#!/usr/bin/env bash

# Set the correct resolution and orientation for my two monitors
xrandr --output DVI-I-0 \
    --off --output DVI-I-1 \
    --off --output HDMI-0 --off \
    --output DP-0 --off \
    --output DP-1 --off \
    --output DP-2 --mode 3840x2160 --pos 0x0 --rotate normal --primary \
    --output DP-3 --off \
    --output DP-4 --mode 3840x2160 --pos 3840x0 --rotate normal \
    --output DP-5 --off

# Load Xresources file
# https://wiki.archlinux.org/index.php/X_resources#xinitrc
xresources="$HOME/.config/X11/desktop_Xresources"
[[ -f $xresources ]] && xrdb -merge -I$HOME $xresources

xsetroot -xcf /usr/share/icons/Vanilla-DMZ/cursors/left_ptr 32
