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

alias a="ls -1sh --color"
alias :x="exit"
alias :q="exit"
alias amplayer='mplayer -vo none'
alias apts='aptitude search'
alias egrep='egrep --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias lss='ls -Srsh -1'
alias make='make -j4'
alias mapdvp='setxkbmap -layout us -variant dvp'
alias mapus='setxkbmap us'
alias sagp='sudo apt-get purge'
alias sagr='sudo apt-get remove'
alias sai='sudo apt-get install'
alias sl='svn log -l'
alias svim='sudo vim'
alias todoc='todo +children'
alias up='sudo apt-get update && sudo apt-get upgrade'
alias wake-server='wakeonlan 00:02:3f:16:16:ab'
alias screens='screen -list'
alias mutt='mutt -f email'
