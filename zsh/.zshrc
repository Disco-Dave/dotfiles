source $ZSH/oh-my-zsh.sh

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
zstyle :compinstall filename '$HOME/.zshrc'
stty -ixon
setopt correct
autoload -U colors && colors

# Enable plugins
plugins=(git zsh-syntax-highlighting zsh-autosuggestions history-substring-search)
