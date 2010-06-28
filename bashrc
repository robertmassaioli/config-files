# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth
export HISTSIZE=100000
export HISTIGNORE="a:[bf]g:exit:history"
#export HISTFILESIZE=100000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
export TERM=xterm-256color
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]'
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w'
fi

if [ "\$(type -t __git_ps1)" ]; then
  PS1="${PS1} \$(__git_ps1 '(%s)')"
fi

if [ "$color_prompt" = yes ]; then
  PS1="$PS1\[\033[00m\]\$ "
else
  PS1="$PS1\$ "
fi

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# The following is for devtodo
TODO_OPTIONS="--timeout --summary"

cd()
{
  if builtin cd "$@"; then
    devtodo ${TODO_OPTIONS}
  fi
}

pushd()
{
  if builtin pushd "$@"; then
    devtodo ${TODO_OPTIONS}
  fi
}

popd()
{
  if builtin popd "$@"; then
    devtodo ${TODO_OPTIONS}
  fi
}

# Run todo initially upon login
devtodo ${TODO_OPTIONS}

# useful bash functions written by me
g()
{
  files=""
  dir=""
  for i in "$@"
  do
    if [ -d "$i" ]; then
      if [ "x$dir" = "x" ]; then
        dir="$i"
      else
        echo "Error: more than one dir [$dir and $i] only one allowed"
        return 1
      fi
    elif [ -r "$i" ]; then
      files="$i $files"
    fi
  done

  files=`echo "$files" | sed 's/[\ ]*$//'`
  if [ "x$files" != "x" ]; then
    editor "$files"
  fi

  if [ "x$dir" != "x" ]; then
    cd "$dir"
  fi
}

swap()
{
  if [ $# -ne 2 ]; then
    echo "Error: Two files must be given to swap."
    return 1
  fi

  temp=`mktemp swap.XXXXX`
  mv "$1" "$temp"
  mv "$2" "$1"
  mv "$temp" "$2"
  # the mv's automatically delete the tmp file
}


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi

export DEBFULLNAME="Robert Massaioli"
export DEBMAIL="robertmassaioli@gmail.com"
export GPGKEY=FF027F05

# for the PicKit 2 Programmer
PATH=$PATH:/usr/share/pk2
export PATH=$PATH:/opt/os161/bin

export PATH=$PATH:~/.cabal/bin

export SVN_EDITOR=editor

GPG_TTY=`tty`
export GPG_TTY

. ~/.xsettings
