call plug#begin('~/.vim/plugged')
" snippets
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
" tree and tagbar
Plug 'majutsushi/tagbar'
Plug 'scrooloose/nerdtree' 
" edit text
Plug 'ervandew/supertab'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'godlygeek/tabular'

" tmux
Plug 'christoomey/vim-tmux-navigator'

" colorschemes
"
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'
Plug 'w0ng/vim-hybrid'
Plug 'archSeer/colibri.vim'
Plug 'nanotech/jellybeans.vim'
" R ulti
Plug 'jalvesaq/nvim-r'
" buffers and files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" js and node
Plug 'ternjs/tern_for_vim'
Plug 'jelera/vim-javascript-syntax'
Plug 'guileen/vim-node-dict'
" fuzzy search
Plug 'mileszs/ack.vim'
" autotagging need config
" Plug 'ludovicchabant/vim-gutentags'
"python autocomplete
" Plug 'davidhalter/jedi-vim'
" linter
Plug 'w0rp/ale'
" web tag closin
Plug 'alvan/vim-closetag'
" c and cpp complete using clang
" Plug 'Rip-Rip/clang_complete'
" java complete
" Plug 'artur-shaik/vim-javacomplete2'
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
" Plugin for deoplete
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-jedi'
Plug 'Shougo/neco-syntax'

call plug#end()

set nocompatible
set encoding=utf-8
set foldmethod=syntax
set path+=**
set nofoldenable
set bs=2          " backspace should work as we expect
set bg=dark
if has('gui_running')
    colorscheme gruvbox
    set bg=dark
endif
"basic config
if (has("termguicolors"))
    set termguicolors
    if &term =~# '^screen'
        let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
        let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"
    endif
endif
if (&term =~ '256color')
    set t_ut=
endif
set mouse=a
" let c_syntax_for_h=0
set tabpagemax=100
set autoindent
set autowrite
set autoread
set wildmenu
set ruler         " show cursor position in bottom line
set nu            " show line number
" set rnu "show relative number
set cursorline
set hlsearch      " highlight search result
" y and d put stuff into system clipboard (so that other apps can see it)
set clipboard=unnamed,unnamedplus
set nowrap
set textwidth=0
set cindent
set ttimeout
set timeoutlen=500
set ttimeoutlen=100 " timeout when keypress belong to any combos

" Tab related stuffs
set shiftround    " when shifting non-aligned set of lines, align them to next tabstop
set smarttab

set listchars=tab:>-,trail:·
set list

" Searching
set incsearch     " show first match when start typing
set ignorecase    " default should ignore case
set smartcase     " use case sensitive if I use uppercase

" autocomplete configuration
set complete-=i
set complete+=t
" set complete+=k
set lazyredraw
set completeopt=menu
set dictionary+=/usr/share/dict/words
set tags=./tags
" 2 line for statusline
set cmdheight=2

let mapleader=','
let maplocalleader='\\'

colorscheme gruvbox
let g:airline_theme='gruvbox'
" plugin config
" ack.vim config
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif
" keybind maps
" tagbar map and config
nmap <leader>t :TagbarToggle<cr>
" nerdtree map and config
nmap <leader>nt :NERDTreeToggle<cr>
" fzf binding
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>a :Ag<cr>
nnoremap <leader>f :Files<CR>
" fugititve map
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gf :Gpull<CR>
nnoremap <leader>gp :Gpush<CR>
" map key for fast split-switching
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l
nmap <C-h> <C-w>h
" map key for saving, nohl and make
nmap <leader>w :w<cr>
nmap <leader><space> :nohl<cr>
nmap <leader>m :make<cr>
" mapped key and :
nnoremap ; :
vnoremap <silent> <TAB> >gv
vnoremap <silent> <S-TAB> <gv
" copy to end of line
nnoremap Y y$
" " copy to system clipboard
nnoremap gy "+y
" " copy whole file to system clipboard
nnoremap gY gg"+yG

autocmd FileType c      call CSET()
autocmd FileType python call PYSET()
autocmd FileType javascript call JSSET()
