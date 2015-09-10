set nocompatible " don't need to be compatible with old vim
filetype off

" Vundle

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'jiangmiao/auto-pairs'
"Plugin 'mattn/emmet-vim'
Plugin 'bling/vim-airline'
Plugin 'eagletmt/neco-ghc'
Plugin 'altercation/vim-colors-solarized'
Plugin 'tomtom/tcomment_vim'
Plugin 'davidhalter/jedi-vim'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'
Plugin 'tpope/vim-fugitive'
Plugin 'mattn/gist-vim'
Plugin 'mattn/webapi-vim'
Plugin 'godlygeek/csapprox'
Plugin 'fs111/pydoc.vim'
" if you use Vundle, load plugins:
 Bundle 'ervandew/supertab'
 Bundle 'Valloric/YouCompleteMe'


" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'
" Colorschemes
Plugin 'nanotech/jellybeans.vim'
Plugin 'reedes/vim-colors-pencil'
call vundle#end()
filetype plugin indent on
syntax on " show syntax highlighting
set t_Co=256
set encoding=utf-8
set fileencoding=utf-8
let g:airline_powerline_fonts = 2
set autoindent " set auto indent
set ts=4 " set indent to 4 spaces
set shiftwidth=2
set expandtab " use spaces, not tab characters
set relativenumber " show relative line numbers
set showmatch " show bracket matches
set ignorecase " ignore case in search
set hlsearch " highlight all search matches
set cursorline " highlight current line
set smartcase " pay attention to case when caps are used
set incsearch " show search results as I type
set title " chanfe the terminal's title
set ttimeoutlen=100 " decrease timeout for faster insert with 'O'
set vb " enable visual bell (disable audio bell)
set ruler " show row and column in footer
set scrolloff=2 " minimum lines above/below cursor
set laststatus=2 " always show status bar
"set list listchars=tab:»·,trail:·,eol:¬ " show extra space characters
set nofoldenable " disable code folding
set wildmenu " enable bash style tab completion
set number " Show number (if realative number is set than show current file number)
set hidden
set background=dark
let g:airline_theme= 'powerlineish'
set statusline=\ \%f%m%r%h%w\ ::\ %y\ [%{&ff}]\%=\ [%p%%:\ %l/%L]\ 
let mapleader = ","

map <leader>q :q<CR>
map <leader>w :w<CR>
map <leader>n :bn<CR>
map <leader>t ::NERDTreeToggle<CR>
map <leader>b :TagbarToggle<CR>


autocmd FileType python map <leader>e :!clear; python %<CR>
autocmd FileType haskell map <leader>e :!clear && runhaskell %<CR>
autocmd FileType java map <leader>e :!clear && javac % && java %:r<CR>
autocmd FileType markdown map <leader>e :!clear && rake preview<CR>


inoremap jk <Esc>
inoremap <C-q> <Esc>:q<CR>
nnoremap j gj
nnoremap k gk
" Normal mode mapping
noremap <C-s> :w<CR>
noremap <C-q> :q<CR>


" Stop using the arrow key !!!!!
	" Normal mode mapping
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
noremap h <NOP>
noremap j <NOP>
noremap k <NOP>
noremap l <NOP>
" Home row windows mapping.
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Spell check.
noremap <f5> :set spell<CR>

"Move visual block 
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv
vnoremap L <Esc>`<<C-v>`>odp`<<C-v>`>lol
vnoremap H <Esc>`<<C-v>`>odhP`<<C-v>`>hoh

autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" Using spell option in mutt files.
set spelllang=en_us,fr
au BufRead,BufNewFile *mutt* set filetype=mail
autocmd FileType mail set spell


set encoding=utf-8

colorscheme monokai

" " make YCM compatible with UltiSnips (using supertab)
 let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
 let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
 let g:SuperTabDefaultCompletionType = '<C-n>'
 let g:ycm_semantic_triggers = {'haskell' : ['.']}
" " better key bindings for UltiSnipsExpandTrigger
 let g:UltiSnipsExpandTrigger = "<tab>"
 let g:UltiSnipsJumpForwardTrigger = "<tab>"
 let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:jedi#usages_command = "<leader>u"

augroup collumnLimit
  autocmd!
  autocmd BufEnter,WinEnter,FileType python
        \ highlight CollumnLimit ctermbg=DarkGrey guibg=DarkGrey
  let collumnLimit = 79 " feel free to customize
  let pattern =
        \ '\%<' . (collumnLimit+1) . 'v.\%>' . collumnLimit . 'v'
  autocmd BufEnter,WinEnter,FileType python
        \ let w:m1=matchadd('CollumnLimit', pattern, -1)
augroup END
let $PATH = $PATH . ':' . expand("~/.cabal/bin")
