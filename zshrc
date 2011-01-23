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

setopt extendedglob
unsetopt CASE_GLOB

# make the pushd commands second nature
DIRSTACKSIZE=200
setopt autopushd pushdsilent pushdtohome

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
alias up='sudo apt-get -y update && sudo apt-get -y upgrade'
alias wake-server='wakeonlan 00:02:3f:16:16:ab'
alias screens='screen -list'
alias mutt='mutt -f email'
alias xs='~/.xsettings'
alias p='pushd'
alias o='popd'
alias dirs='dirs -v'
alias cb='cabal build'
alias cccb='cabal configure && cabal build'
alias hibernate='sudo pm-hibernate --quirk-vbestate-restore'
alias last='last -a'

# Suffix Aliases
alias -s -- txt='vim'
alias -s -- hs='vim'
alias -s -- c='vim'
alias -s -- h='vim'

export EDITOR="/usr/local/bin/vim"
export MAVEN_OPTS="-ea"

export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/Interests/Chrome/depot_tools:$PATH"
