# The following lines were added by compinstall
zstyle :compinstall filename '/home/robert/.zshrc'

setopt promptsubst

autoload -U promptinit
promptinit

prompt wunjo

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

alias -g a="ls -1sh --color"
alias -g :x="exit"
alias -g :q="exit"
alias -g amplayer='mplayer -vo none'
alias -g apts='aptitude search'
alias -g egrep='egrep --color=auto'
alias -g grep='grep --color=auto'
alias -g ls='ls --color=auto'
alias -g lss='ls -Srsh -1'
alias -g make='make -j4'
alias -g mapdvp='setxkbmap -layout us -variant dvp'
alias -g mapus='setxkbmap us'
alias -g sagp='sudo apt-get purge'
alias -g sagr='sudo apt-get remove'
alias -g sai='sudo apt-get install'
alias -g sl='svn log -l'
alias -g svim='sudo vim'
alias -g todoc='todo +children'
alias -g up='sudo apt-get update && sudo apt-get upgrade'
alias -g wake-server='wakeonlan 00:02:3f:16:16:ab'
