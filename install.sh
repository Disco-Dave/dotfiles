#!/bin/bash

# Exit this script if any command returns a non-zero exit status
set -e


# XDG Directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME"


# Install all required packages via pacman
sudo pacman -Syu --needed --noconfirm \
    alacritty arc-gtk-theme arc-icon-theme atril base-devel blueman bluez \
    bluez-utils dmenu feh firefox gcc git git glib2 gmp gvfs haskell-x11 \
    haskell-x11-xft htop libnotify lxsession make mpv neovim network-manager-applet \
    nodejs npm openssh p7zip pasystray pavucontrol picom pulseaudio-bluetooth \
    python-gobject python-xdg ranger redshift stalonetray xclip \
    xcursor-vanilla-dmz xfce4-notifyd xfce4-power-manager xfce4-screenshooter \
    zlib zsh zsh-autosuggestions zsh-completions zsh-history-substring-search \
    zsh-syntax-highlighting ttf-hack xdotool

if [[ ! -f "/usr/bin/yay" ]]; then
    curl -o yay.tar.gz https://aur.archlinux.org/cgit/aur.git/snapshot/yay.tar.gz
    tar xf yay.tar.gz
    (cd yay; makepkg -si)
    rm -rf yay yay.tar.gz
fi

if [[ -d "$XDG_CONFIG_HOME/dotfiles" ]]; then
    cd "$XDG_CONFIG_HOME/dotfiles"
    git pull
else
    git clone https://github.com/Disco-Dave/dotfiles.git "$XDG_CONFIG_HOME/dotfiles"
    cd "$XDG_CONFIG_HOME/dotfiles"
    git remote set-url origin git@github.com:Disco-Dave/dotfiles.git
fi


# Setup npm
if [[ ! -f "$XDG_CONFIG_HOME/npm/npmrc" ]]; then
    mkdir -p "$XDG_CONFIG_HOME/npm"
    echo "prefix=$XDG_DATA_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "cache=$XDG_CACHE_HOME/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "tmp=\$XDG_RUNTIME_DIR/npm" >> "$XDG_CONFIG_HOME/npm/npmrc"
    echo "init-module=$XDG_CONFIG_HOME/npm/config/npm-init.js" >> "$XDG_CONFIG_HOME/npm/npmrc"
fi
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc"


# Install haskell
# https://docs.haskellstack.org/en/stable/README/
export STACK_ROOT="$XDG_DATA_HOME/stack"
if [[ ! -f "/usr/local/bin/stack" ]]; then
    curl -sSL https://get.haskellstack.org/ | sh
    stack setup
fi

ln -sfn "$(pwd)/ghc/ghci" "$HOME/.ghci"


# Link up git config
ln -sfn "$(pwd)/git" "$XDG_CONFIG_HOME/git"


# Setup zsh
echo "export ZDOTDIR=$XDG_CONFIG_HOME/zsh" | sudo tee /etc/zsh/zshenv > /dev/null
mkdir -p "$XDG_CONFIG_HOME/zsh"
mkdir -p "$XDG_DATA_HOME/zsh/plugins"
mkdir -p "$XDG_CACHE_HOME/zsh"

if [[ ! -d "$XDG_DATA_HOME/zsh/plugins/zsh-git-prompt" ]]; then
    git clone https://github.com/olivierverdier/zsh-git-prompt.git \
        $XDG_DATA_HOME/zsh/plugins/zsh-git-prompt
    (cd $XDG_DATA_HOME/zsh/plugins/zsh-git-prompt; stack install)
fi

ln -sfn "$(pwd)/zsh/zprofile" "$XDG_CONFIG_HOME/zsh/.zprofile"
ln -sfn "$(pwd)/zsh/zshenv" "$XDG_CONFIG_HOME/zsh/.zshenv"
ln -sfn "$(pwd)/zsh/zshrc" "$XDG_CONFIG_HOME/zsh/.zshrc"

if [ "$SHELL" != "/bin/zsh" ]; then
    chsh -s "/bin/zsh" "$USER"

    rm -rf "$HOME/.bash_logout"
    rm -rf "$HOME/.bash_profile"
    rm -rf "$HOME/.bash_history"
    rm -rf "$HOME/.bashrc"
fi


# Setup ssh-agent
mkdir -p "$HOME/.ssh"
if [[ ! -f "$HOME/.ssh/config" ]]; then
    echo "AddKeysToAgent yes" >> "$HOME/.ssh/config"
fi
if [[ ! -f "$HOME/.pam_environment" ]]; then
    echo "SSH_AUTH_SOCK DEFAULT=\"${XDG_RUNTIME_DIR}/ssh-agent.socket\"" >> "$HOME/.pam_environment"
fi
mkdir -p "$XDG_CONFIG_HOME/systemd/user"
ln -sfn "$(pwd)/systemd/user/ssh-agent.service" "$XDG_CONFIG_HOME/systemd/user/ssh-agent.service"
systemctl --user enable --now ssh-agent.service


# Setup neovim
npm install -g neovim

ln -sfn "$(pwd)/nvim" "$XDG_CONFIG_HOME/nvim"
mkdir -p "$XDG_DATA_HOME/nvim/backup"
mkdir -p "$XDG_DATA_HOME/nvim/swap"
mkdir -p "$XDG_DATA_HOME/nvim/undo"
mkdir -p "$XDG_DATA_HOME/nvim/language-servers"

if [[ ! -f "$XDG_DATA_HOME/nvim/site/autoload/plug.vim" ]]; then
    sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
           https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    nvim +PlugInstall +qall
fi


# Setup XMonad and XMobar
export XMONAD_CONFIG_HOME="$XDG_CONFIG_HOME"/xmonad
export XMONAD_DATA_HOME="$XDG_DATA_HOME"/xmonad
export XMONAD_CACHE_HOME="$XDG_CACHE_HOME"/xmonad

ln -sfn "$(pwd)/xmobar" "$XDG_CONFIG_HOME/xmobar"
ln -sfn "$(pwd)/xmonad" "$XDG_CONFIG_HOME/xmonad"
mkdir -p "$XMONAD_DATA_HOME" "$XMONAD_CACHE_HOME"
mkdir -p "$XDG_CONFIG_HOME/xmonad/sources"

if [[ ! -d "$XDG_CONFIG_HOME/xmonad/sources/xmonad-git" ]]; then
    git clone https://github.com/xmonad/xmonad "$XDG_CONFIG_HOME/xmonad/sources/xmonad-git"
fi
if [[ ! -d "$XDG_CONFIG_HOME/xmonad/sources/xmonad-contrib-git" ]]; then
    git clone https://github.com/xmonad/xmonad-contrib "$XDG_CONFIG_HOME/xmonad/sources/xmonad-contrib-git"
fi

(
    cd "$XDG_CONFIG_HOME/xmonad"
    stack install

    cd "$XDG_CONFIG_HOME/xmobar"
    stack install
)

rm -rf "$HOME"/.xmonad
"$HOME"/.local/bin/xmonad --recompile

ln -sfn "$(pwd)"/X11 "$XDG_CONFIG_HOME"/X11
ln -sfn "$(pwd)/stalonetray" "$XDG_CONFIG_HOME"/stalonetray


# Theming
ln -sfn "$(pwd)/gtk-2.0" "$XDG_CONFIG_HOME/gtk-2.0"
ln -sfn "$(pwd)/gtk-3.0" "$XDG_CONFIG_HOME/gtk-3.0"
ln -sfn "$(pwd)/icons" "$HOME/.icons"


# Setup alacritty
ln -sfn "$(pwd)/alacritty" "$XDG_CONFIG_HOME/alacritty"

# Setup bluetooth
sudo systemctl enable bluetooth.service

# Setup picom
ln -sfn "$(pwd)/picom" "$XDG_CONFIG_HOME/picom"

# Setup ranger
ln -sfn "$(pwd)/ranger" "$XDG_CONFIG_HOME/ranger"
