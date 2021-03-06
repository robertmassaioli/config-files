" An example for a vimrc file.
"
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last change:	2008 Jul 02
"
" To use it, copy it to
"     for Unix and OS/2:  ~/.vimrc
"	      for Amiga:  s:.vimrc
"  for MS-DOS and Win32:  $VIM\_vimrc
"	    for OpenVMS:  sys$login:.vimrc

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Is no modelines really what I want?
set modelines=0

" Tab properties
set tabstop=3
set softtabstop=3
set shiftwidth=3
set expandtab

" Change the leader to be ,
let mapleader = ","

" Bar
set laststatus=2

" New Features
set undofile
set relativenumber

" Tame search
nnoremap / /\V
nnoremap / /\v
set ignorecase
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch
nnoremap <leader><space> :noh<cr>

nnoremap j gj
nnoremap k gk

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Get rid of that help key
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" OMG Yes, save when you loose focus
au FocusLost * :wa

" Some of my custom commands
nnoremap <leader>ft Vatzf
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>
nnoremap <leader>w <C-w>v<C-w>l
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>
nnoremap <leader>t :tabedit<CR>
let g:ackprg="ack-grep -H --nocolor --nogroup --column"
nnoremap <leader>a :Ack
nnoremap <leader>n :NERDTreeToggle<CR>

" Moving bettween my splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Save on loose focus

"if has("vms")
"  set nobackup		" do not keep a backup file, use versions instead
"else
"  set backup		" keep a backup file
"endif
set nobackup

set encoding=utf-8
set textwidth=100
set autoindent		" always set autoindenting on
set wildmenu
set wildmode=list:longest
set ttyfast

set history=100		" keep 50 lines of command line history
set ruler		      " show the cursor position all the time
set showcmd		    " display incomplete commands

" For Win32 GUI: remove 't' flag from 'guioptions': no tearoff menu entries
" let &guioptions = substitute(&guioptions, "t", "", "g")

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
   set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
syntax on

" Only do this part when compiled with support for autocommands.
if has("autocmd")

   " Enable file type detection.
   " Use the default filetype settings, so that mail gets 'tw' set to 72,
   " 'cindent' is on in C files, etc.
   " Also load indent files, to automatically do language-dependent indenting.
   filetype plugin indent on

   " Put these in an autocmd group, so that we can delete them easily.
   augroup vimrcEx
      au!

      " For all text files set 'textwidth' to 78 characters.
      autocmd FileType text setlocal textwidth=100

      autocmd FocusLost * :w

      " When editing a file, always jump to the last known cursor position.
      " Don't do it when the position is invalid or when inside an event handler
      " (happens when dropping a file on gvim).
      " Also don't do it when the mark is in the first line, that is the default
      " position when opening a file.
      autocmd BufReadPost *
               \ if line("'\"") > 1 && line("'\"") <= line("$") |
               \   exe "normal! g`\"" |
               \ endif

   augroup END

endif " has("autocmd")


" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
   command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
            \ | wincmd p | diffthis
endif


" set bg=dark
colorscheme adrian
" colorscheme BlackSea

au BufWinLeave *.* mkview
au BufWinEnter *.* silent loadview
au BufEnter    *.hs compiler ghc

let g:haddock_browser = "/usr/bin/google-chrome"
let g:haddock_docdir = "/usr/share/doc/ghc6-doc/html/"
let g:ghc = "/usr/bin/ghc"

" For dvorak

" Turn off highlighting with space
:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>
