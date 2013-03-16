set nocompatible          " get rid of Vi compatibility mode. SET FIRST!
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set mouse=a

set gfn=Inconsolata\ 15

" <vundle>
filetype off " for vundle https://github.com/gmarik/vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" vundle's my bundles
Bundle 'ujihisa/neco-ghc'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'scrooloose/syntastic'
Bundle 'Shougo/vimproc'
" Bundle 'git://github.com/bitc/vim-hdevtools'

" http://bloerg.net/2012/08/23/updates-in-vim-land.html
Bundle 'mileszs/ack.vim'
Bundle 'Raimondi/delimitMate'
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/neosnippet'
Bundle 'Lokaltog/vim-powerline'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-fugitive'
Bundle 'nvie/vim-flake8'
Bundle 'matze/dwm.vim'

filetype on
" </vundle>

" enable filetype detection, plus loading of filetype plugins
filetype plugin indent on

" https://github.com/jnwhiteh/vim-golang
set rtp+=$GOROOT/misc/vim

" filetype plugin indent on " filetype detection[ON] plugin[ON] indent[ON]

syntax on
" set cindent " TODO


" TODO wtf is this
" use ghc functionality for haskell files
au Bufenter *.hs compiler ghc

" configure browser for haskell_doc.vim
let g:haddock_browser = "/usr/bin/firefox"

" pathogen
call pathogen#infect()

" org-vim
let g:org_heading_shade_leading_stars = 1


set t_Co=256              " enable 256-color mode.
syntax enable             " enable syntax highlighting (previously syntax on).
colorscheme desert        " set colorscheme
set number                " show line numbers
set laststatus=2          " last window always has a statusline
filetype indent on        " activates indenting for files
set nohlsearch            " Don't continue to highlight searched phrases.
set incsearch             " But do highlight as you type your search.
" set ignorecase            " Make searches case-insensitive.
set ruler                 " Always show info along bottom.
set autoindent            " auto-indent
set tabstop=4             " tab spacing
set softtabstop=4         " unify
set shiftwidth=4          " indent/outdent by 4 columns
set shiftround            " always indent/outdent to the nearest tabstop
set expandtab             " use spaces instead of tabs
" set smarttab              " use tabs at the start of a line, spaces elsewhere
set wrap                  " wrap text

" http://andrewradev.com/2011/04/26/my-vim-workflow-basic-moves/
" The first one is not going through visual lines. I strongly prefer line
" wrapping and I always end up trying to go to the next/previous visual line,
" only to be sent to the next/previous real line and start thinking about how
" to get back.
nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

" I’d much rather like to be able to move through lines with a bigger step. An
" obvious mapping to that end (or at least obvious in hindsight) is:
" This one is a lot more intrusive, though. J is used reasonably often for
" joining lines together, but instead of that, I just use :join. As for K,
" it’s mapped to showing the manual for the word under the buffer, but it
" really doesn’t seem like something that’s used often enough to warrant a
" single key on the home row (and I usually prefer googling anyway).
nmap J 5j
nmap K 5k
xmap J 5j
xmap K 5k

" To move efficiently between splits, I use these simple mappings:
nmap gh <C-w>h
nmap gj <C-w>j
nmap gk <C-w>k
nmap gl <C-w>l

" On the other hand, gt and gT (click) are a horrible finger combo to me,
" since they need to be pressed with the same hand. Adding this mapping was
" essential for me to be able to whiz through tabs just as easily as with
" splits:
nmap <C-l> gt
nmap <C-h> gT

" As for closing buffers, something I’ve found particularly useful is this:
" A bit more involved, but actually not that difficult to understand. I use
" :tabclose to attempt to close the current tab page. If it’s the last one, an
" error is raised, so I just catch it and quit instead. The particular problem
" I was trying to solve was having a NERDTree opened on each tab. That meant
" that even if I have a single buffer in the tab, a simple :q would not be
" enough. And since :tabc is way too long and doesn’t work for a single page,
" I just came up with a slightly more elaborate combination that does what I
" need and mapped it to QQ.
nnoremap QQ :QuitTab<cr>
command! QuitTab call s:QuitTab()
function! s:QuitTab()
  try
    tabclose
  catch /E784/ " Can't close last tab
    qall
  endtry
endfunction

" https://kaigaraonline.wordpress.com/2011/03/25/ide-for-python-with-vim/
" http://vim.wikia.com/wiki/Omni_completion
"" set ofu=syntaxcomplete#Complete

" http://fromacoder.blogspot.co.il/2011/09/vim-workflow-for-pythondjango.html
" https://github.com/Shougo/neocomplcache
let g:neocomplcache_enable_at_startup = 1

" http://wiki.python.org/moin/Vim
set background=dark

" http://vimdoc.sourceforge.net/htmldoc/usr_40.html
" http://stackoverflow.com/questions/4106137/manage-todo-lots-of-files-with-vim
" command Todo noautocmd vimgrep /TODO/j **/*.py<CR>:cw<CR>

" http://scipy-lectures.github.com/advanced/debugging/index.html#id4
" In your vimrc (binds F5 to pyflakes):
autocmd FileType python let &mp = 'echo "*** running % ***" ; pyflakes %'
autocmd FileType tex,mp,rst,python imap <Esc>[15~ <C-O>:make!^M
autocmd FileType tex,mp,rst,python map  <Esc>[15~ :make!^M
autocmd FileType tex,mp,rst,python set autowrite

let g:syntastic_python_checker = 'pyflakes'

" https://github.com/bitc/vim-hdevtools
" au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
" au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
