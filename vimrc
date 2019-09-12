" Carl Raiden Worley's .vimrc

" allows non features vi wouldn't like
set nocompatible

" allows standard backspacing
set backspace=indent,eol,start

" attempts to determine filetype and allows intelligent behavior
filetype indent plugin on

" enable sytax highlighting
syntax on

" fancy command line completion
set wildmenu

" use case insensitive search, unless caps are used in the search term
set ignorecase
set smartcase

" when starting a new line and there's no filetype-specific indentation,
" just use the same indent as the line you're on
set autoindent

" enables use of mouse in all modes
set mouse=a

" displays line numbers on the left
set number

" tab settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" colored column past 80
set colorcolumn=80

