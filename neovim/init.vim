call plug#begin('~/.config/nvim/plugged')

Plug 'junegunn/seoul256.vim'
Plug 'junegunn/vim-easy-align'

Plug 'mbbill/undotree'
Plug 'easymotion/vim-easymotion'
Plug 'chrisbra/NrrwRgn'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'davidhalter/jedi-vim'

" Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
" Plug 'scrooloose/syntastic'

Plug 'osyo-manga/vim-watchdogs'
Plug 'thinca/vim-quickrun'
Plug 'osyo-manga/shabadou.vim'
Plug 'jceb/vim-hier'
Plug 'dannyob/quickfixstatus'

Plug 'benekastah/neomake'

Plug 'eagletmt/neco-ghc'
Plug 'eagletmt/ghcmod-vim'
" Plug 'bitc/vim-hdevtools' " , { 'for': 'hs' }
Plug 'lukerandall/haskellmode-vim' " , { 'for': 'hs' }
Plug 'Shougo/vimproc'
Plug 'dan-t/vim-hsimport'

Plug 'majutsushi/tagbar'

Plug 'honza/vim-snippets'
Plug 'Shougo/neosnippet.vim'

Plug 'Shougo/neocomplete.vim'

Plug 'morhetz/gruvbox'
Plug 'airblade/vim-gitgutter'

Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'

call plug#end()

syntax on
filetype plugin on

autocmd! BufWritePost * Neomake

let g:haddock_browser = "firefox"

" set statusline+=%#warningmsg#
" set statusline+=%#{SyntasticStatuslineFlag()}
" set statusline+=%*
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

setlocal omnifunc=necoghc#omnifunc

let g:necoghc_enable_detailed_browse = 1

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif


set ruler
set number


" Up and down through a wrapped line
" https://stackoverflow.com/questions/20975928/moving-the-cursor-through-long-soft-wrapped-lines-in-vim
noremap <expr> j (v:count == 0 ? 'gj' : 'j')
noremap <expr> k (v:count == 0 ? 'gk' : 'k')

hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

let g:necoghc_enable_detailed_browse = 1

" set tabstop=4 " convert tabs into tabstop spaces
" set noexpandtab " don't convert tabs into spaces
" " set softtabstop=4
" set shiftwidth=4
" set shiftround
set noexpandtab

autocmd FileType cabal setlocal softtabstop=4 shiftwidth=4 tabstop=4 expandtab
autocmd FileType haskell setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab
