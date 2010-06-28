# This is my aliases file complete with everything
# this should be run by my bashrc

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ] && [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias a='ls -1sh'
alias lss='ls -Srsh -1'

alias apts='aptitude search'
alias sai='sudo apt-get install'
alias sagr='sudo apt-get remove'
alias sagp='sudo apt-get purge'
alias sl='svn log -l'
export uni='robertm@cse.unsw.edu.au'
export home='192.168.0.97'
export homeAway='massaioli.homelinux.com'
alias ack='ack-grep'
alias make='make -j4'
alias wake-server='wakeonlan 00:02:3f:16:16:ab'
alias amplayer='mplayer -vo none'
alias up='sudo apt-get update && sudo apt-get upgrade'
export csesoc='robertm@csesoc-server.cse.unsw.edu.au'
export unicsesoc='csesoc@cse.unsw.edu.au'
export alioth='robertmassaioli-guest@collab-maint.alioth.debian.org'
alias :q='exit'
alias :x='exit'
alias svim='sudo vim'

alias todoc='todo +children'

alias mapus='setxkbmap us'
alias mapdvp='setxkbmap -layout us -variant dvp'
