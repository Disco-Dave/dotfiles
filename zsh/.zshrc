ZSH_THEME="clean-newline"

HYPHEN_INSENSITIVE="true"

export UPDATE_ZSH_DAYS=30
export ASPNETCORE_ENVIRONMENT=Development
export RUST_SRC_PATH=/home/disco/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/
export ERL_AFLAGS="-kernel shell_history enabled"
export NODE_ENV="development"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

plugins=(git zsh-syntax-highlighting zsh-autosuggestions history-substring-search)

source $ZSH/oh-my-zsh.sh

function targz { tar -pczf "$1".tar.gz "${@:2}" }

# Autocomplete haskell's stack
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(stack --bash-completion-script stack)"

# Other
alias vscan='clamscan --recursive=yes --infected -v --exclude-dir="^/sys|^/dev /\|^/backup|^/data"'
alias weather='curl wttr.in/Mechanicsburg'
alias refresh_screens='xrandr --output DP-3 --off --output DVI-I-0 --off --output HDMI-0 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-5 --off --output DP-4 --mode 3840x2160 --pos 5760x0 --rotate normal --output DVI-I-1 --off --output DP-2 --primary --mode 3840x2160 --pos 1920x0 --rotate normal --output DP-1 --off --output DP-0 --off'

zstyle ':completion:*' completer _complete _ignored
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
HISTFILE=~/.histfile
HISTSIZE=100000000000000
SAVEHIST=100000000000000
bindkey -v

bindkey '^[[Z' reverse-menu-complete

stty -ixon

setopt correct

autoload -U colors && colors
export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r?$reset_color (Yes, No, Abort, Edit) "


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# tabtab source for electron-forge package
# uninstall by removing these lines or running `tabtab uninstall electron-forge`
[[ -f /home/disco/.local/share/npm-global/lib/node_modules/electron-forge/node_modules/tabtab/.completions/electron-forge.zsh ]] && . /home/disco/.local/share/npm-global/lib/node_modules/electron-forge/node_modules/tabtab/.completions/electron-forge.zsh


## source $ZSH/oh-my-zsh.sh
## 
## # Use vim keybindings
## bindkey -v
## 
## # History settings
## export HISTFILE="$XDG_DATA_HOME"/zsh/history
## export HISTSIZE=9999999999
## export SAVEHIST=9999999999
## 
## # Auto completion settings
## autoload -Uz compinit
## compinit -d $XDG_CACHE_HOME/zsh/zcompdump-$ZSH_VERSION
## bindkey '^[[Z' reverse-menu-complete
## HYPHEN_INSENSITIVE="true"
## COMPLETION_WAITING_DOTS="true"
## 
## # Set ZSH theme
## ZSH_THEME="clean-newline"
## zstyle ':completion:*' completer _complete _ignored
## zstyle :compinstall filename '$XDG_DATA_HOME/zsh/.zshrc'
## stty -ixon
## setopt correct
## autoload -U colors && colors
## export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r?$reset_color (Yes, No, Abort, Edit) "
## 
## # Enable plugins
## plugins=(git zsh-syntax-highlighting zsh-autosuggestions history-substring-search)
