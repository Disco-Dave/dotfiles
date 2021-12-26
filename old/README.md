## Personal dotfiles
Repository of my personal dotfiles and my own personal notes for installing Arch Linux.

## Table of contents
* [Goals](#goals)
* [Overview](#overview)
    * [Overview of components](#overview-of-components)
    * [Overview of styles](#overview-of-styles)
* [Getting started](#getting-started)
    * [Pre-install](#pre-install)
    * [Post-install](#post-install)
* [TODOs](#todos)

## Goals
* Organized
    * Create an install script that can be ran once to configure my system to my exact needs. 
      The install script should also be able to be ran multiple times without negative consequences.
    * Files should be well formatted and commented. Include links to the arch wiki or other
      documentation as needed.
    * Try to keep dotfiles out of the home directory. Instead try to follow [XDG Base Directory](https://wiki.archlinux.org/index.php/XDG_Base_Directory) as much as possible.
* Simplicity
    * Keep scripts and configurations as small and simple as possible.
    * Avoid using command aliases since it clutters the `zshrc` file and prevents me from learning the tool.
    * Only add necessary plugins to `neovim` or capabilities to `xmonad`.
    * Use the same configuration files for my laptop and desktop.
* Efficient
    * `xmonad` should automatically layout my windows to my exact needs without intervention.
    * Utilize the terminal and keyboard as much as possible.
    * Avoid making changes purely for aesthetics when they interfere with efficiency. For example: gaps between windows or transparent terminals.

## Overview
#### Overview of components
| Component Type    | Component                                                                                                                 |
|-------------------|---------------------------------------------------------------------------------------------------------------------------|
| Operating System  | [Arch Linux](https://www.archlinux.org/)                                                                                  |
| Boot loader       | [systemd-boot](https://wiki.archlinux.org/index.php/Systemd-boot)                                                         |
| Networking        | [NetworkManager](https://wiki.archlinux.org/index.php/NetworkManager)                                                     |
| Sound             | [Pulseaudio](https://wiki.archlinux.org/index.php/PulseAudio)                                                             |
| Editor            | [Neovim](https://wiki.archlinux.org/index.php/Neovim)                                                                     |
| Shell             | [Zsh](https://wiki.archlinux.org/index.php/Zsh)                                                                           |
| Window Manager    | [xmonad](https://wiki.archlinux.org/index.php/Xmonad)                                                                     |
| Status bar        | [xmobar](https://wiki.archlinux.org/index.php/Xmonad)                                                                     |
| Terminal Emulator | [Alacritty](https://wiki.archlinux.org/index.php/Alacritty)                                                               |
| Launcher          | [dmenu](https://wiki.archlinux.org/index.php/Dmenu)                                                                       |
| Internet Browser  | [Firefox](https://wiki.archlinux.org/index.php/Firefox)                                                                   |
| File Manager      | [ranger](https://wiki.archlinux.org/index.php/Ranger)                                                                     |
| Music Player      | [mpd](https://wiki.archlinux.org/index.php/Music_Player_Daemon) / [ncmpcpp](https://wiki.archlinux.org/index.php/Ncmpcpp) |
| Email             | [neomutt](https://wiki.archlinux.org/index.php/Mutt)                                                                      |
| Video             | [mpv](https://wiki.archlinux.org/index.php/Mpv)                                                                           |
| PDF Viewer        | [atril](https://www.archlinux.org/packages/community/x86_64/atril/)                                                       |

#### Overview of styles
| Style Type    | Selection                                       |
|---------------|-------------------------------------------------|
| Gtk 2/3 Theme | Arc-Dark                                        |
| Font          | Hack / FreeSans                                 |
| Cursor        | Vanilla-DMZ                                     |
| Wallpaper     | [nebula](assets/wallpaper.jpg)                  |
| Colors        | [nord](https://github.com/arcticicestudio/nord) |

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
8. Setup SSH keys as needed.
9. Setup Firefox.
10. Setup [powertop](https://wiki.archlinux.org/index.php/Powertop) if needed.

## TODOs
* Configure E-Mail
    * Configure Neomutt
    * Add an unread email count to xmobar and when clicked opens neomutt
* Configure ncmpcpp and mpd
* Configure weechat
* Find a keybinding for scrolling up and down in alacritty that does not conflict with xmonad or neovim
* Add the number of open windows on current workspace to xmobar. (Helpful when in the Full layout)
* Add a vi-mode status indication my zsh prompt so I can tell when it's in insert or normal mode
* Learn more about layouts in xmonad and research the following:
    * layout groups
    * sublayouts
    * tabbed layouts
    * accordion layouts
    * scratch pads
    * prompts
* Write a script to switch between a light and dark theme
