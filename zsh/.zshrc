export ZSH=$HOME/.config/oh-my-zsh

# Use vim keybindings
bindkey -v

# History settings
export HISTFILE="$XDG_DATA_HOME"/zsh/history
export HISTSIZE=9999999999
export SAVEHIST=9999999999

# Auto completion settings
autoload -Uz compinit
compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '$HOME/.zshrc'
bindkey '^[[Z' reverse-menu-complete
HYPHEN_INSENSITIVE="true"
COMPLETION_WAITING_DOTS="true"

# Set ZSH theme
ZSH_THEME="clean-newline"

# Enable plugins
plugins=(git zsh-syntax-highlighting zsh-autosuggestions history-substring-search)
