# Use vim keybindings
bindkey -v

# History settings
export HISTFILE="$XDG_DATA_HOME"/zsh/history
export HISTSIZE=9999999999
export SAVEHIST=9999999999

# Auto completion settings
autoload -Uz compinit
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
bindkey '^[[Z' reverse-menu-complete
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"

# Set ZSH theme
ZSH_THEME="clean-newline"
zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '$XDG_DATA_HOME/zsh/.zshrc'
stty -ixon
setopt correct
autoload -U colors && colors
export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r?$reset_color (Yes, No, Abort, Edit) "

# Enable plugins
plugins=(git zsh-syntax-highlighting zsh-autosuggestions history-substring-search)


# Package Manager Related
alias cleanvar='sudo paccache -r && sudo pacman -Sc --noconfirm'
alias minstall='makepkg -si'
alias orphans='yay -Qdt'
alias pinstall='sudo pacman -S'
alias remove='sudo pacman -Rs'
alias search='yay -Ss'
alias pfile='pacman -Fs'
alias plist='pacman -Fl'
alias update='yay -Syu --noconfirm && sync && sudo updatedb'
alias updateall='updatemirrors&&update'
alias updatemirrors='sudo $HOME/Documents/dotfiles/rcandscripts/updatemirrors'
alias yinstall='yay -S'

# Nocorrect
alias pacman='nocorrect pacman'
alias aurman='nocorrect aurman'
alias cargo='nocorrect cargo'
alias yay='nocorrect yay'
alias find='nocorrect find'
alias grep='nocorrect grep'
alias take='nocorrect take'
alias locate='nocorrect locate'
alias killall='nocorrect killall'
alias pkill='nocorrect pkill'
alias pgrep='nocorrect pgrep'
alias touch='nocorrect touch'
alias npm='nocorrect npm'
alias rake='nocorrect rake'

# Xmodmap
alias capsescape='xmodmap -e "clear Lock" -e "keycode 0x42 = Escape"'
alias escapenine='xmodmap -e "keycode 81 = Escape"'

# Convience
alias clearswap='sudo swapoff -a && sudo swapon -a'
alias c='clear'
alias ls='ls --color=always'
alias la='ls -lAh'
alias l='ls -lAh'
alias untar='tar -zxvf'

source $ZSH/oh-my-zsh.sh
