## TODO
* Setup neomutt for email
* Add email inbox counter to xmobar
* Setup weechat for IRC

## Personal dotfiles
Repository of my personal dotfiles and my own personal notes for installing Arch Linux.

## Table of contents
* [Overview](#overview)
    * [Overview of components](#overview-of-components)
    * [Overview of styles](#overview-of-styles)
* [Getting started](#getting-started)
    * [Pre-install](#pre-install)
    * [Post-install](#post-install)

## Overview
#### Overview of components
| Component Type   | Component                                |
|------------------|------------------------------------------|
| Operating System | [Arch Linux](https://www.archlinux.org/) |
| Boot loader      | [systemd-boot](https://wiki.archlinux.org/index.php/Systemd-boot) |
| Networking       | [NetworkManager](https://wiki.archlinux.org/index.php/NetworkManager) |
| Sound            | [Pulseaudio](https://wiki.archlinux.org/index.php/PulseAudio) |
| Editor           | [Neovim](https://wiki.archlinux.org/index.php/Neovim) |
| Shell            | [Zsh](https://wiki.archlinux.org/index.php/Zsh) |
| Window Manager   | [xmonad](https://wiki.archlinux.org/index.php/Xmonad) |
| Status bar       | [xmobar](https://wiki.archlinux.org/index.php/Xmonad) |
| Terminal Emulator | [Alacritty](https://wiki.archlinux.org/index.php/Alacritty) |
| Launcher         | [dmenu](https://wiki.archlinux.org/index.php/Dmenu) |
| Internet Browser | [Firefox](https://wiki.archlinux.org/index.php/Firefox) |
| File Manager     | [ranger](https://wiki.archlinux.org/index.php/Ranger) |
| Music Player     | [mpd](https://wiki.archlinux.org/index.php/Music_Player_Daemon) / [ncmpcpp](https://wiki.archlinux.org/index.php/Ncmpcpp) |
| Email            | [neomutt](https://wiki.archlinux.org/index.php/Mutt) |
| Video            | [mpv](https://wiki.archlinux.org/index.php/Mpv) |
| PDF Viewer       | [atril](https://www.archlinux.org/packages/community/x86_64/atril/) |

#### Overview of styles
| Style Type | Selection |
|------------|-----------|
| Gtk 2/3 Theme | Arc-Dark |
| Font | Hack |
| Cursor | Vanilla-DMZ |
| Wallpaper | [nebula](assets/wallpaper.jpg) |
| Colors | [nord](https://github.com/arcticicestudio/nord) |

## Getting started
### Pre-install
1. Download [arch linux iso](https://www.archlinux.org/download/). Write to flash drive with [dd](https://wiki.archlinux.org/index.php/USB_flash_installation_media#Using_dd).
2. Boot computer from flash drive.
3. Reread and follow [official install guide](https://wiki.archlinux.org/index.php/Installation_guide) and take note of the following reminders:
    * Make a `/boot` partition that is 512MiB for EFI
    * Install [util-linux](https://www.archlinux.org/packages/?name=util-linux) and enable [fstrim](https://wiki.archlinux.org/index.php/Solid_state_drive#Periodic_TRIM) for SSDs
    * Install [NetworkManager](https://wiki.archlinux.org/index.php/NetworkManager)
    * Install `neovim`, `man-db`, `man-pages`, and `texinfo`
    * Install [microcode](https://wiki.archlinux.org/index.php/Microcode) if needed
    * Setup systemd-boot [hook](https://wiki.archlinux.org/index.php/Systemd-boot#Automatic_update) and [loader](https://wiki.archlinux.org/index.php/Systemd-boot#Adding_loaders) for arch linux
    * Setup [reflector](https://wiki.archlinux.org/index.php/Reflector)
    * Enable multilib

### Post-install
1. Setup internet connection via `nmtui`
2. Install sudo and create user
    * `pacman -S sudo`
    * `EDITOR=nvim visudo` and uncomment `%wheel ALL=(ALL) ALL`
    * Add user `useradd -m -G wheel my_user_name_here` and set password `passwd my_user_name_here`
3. Configure [xorg](https://wiki.archlinux.org/index.php/Xorg) and display drivers if necessary
    * `sudo pacman -S xorg xorg-xinit xterm`
    * Test that it works with `startx`
4. Configure [pulseaudio](https://wiki.archlinux.org/index.php/PulseAudio#Installation)
    * `sudo pacman -S pulseaudio pulseaudio-alsa alsa-utils pulsemixer`
    * Ensure pulseaudio is started `systemctl --user start pulseaudio`
    * Ensure pulseaudio is umuted via `pulsemixer`
    * Test sound with `speaker-test -c 2`
5. Enable [automatic login](https://wiki.archlinux.org/index.php/Getty#Automatic_login_to_virtual_console)
6. Install dotfiles: `sh -c "$(curl -fsSL https://raw.githubusercontent.com/Disco-Dave/dotfiles/master/install.sh)"`
7. Reboot and hopefully it starts directly into XMonad.
8. Set SSH keys as needed.
9. Set up Firefox.
