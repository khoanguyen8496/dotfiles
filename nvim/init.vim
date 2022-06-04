" - Avoid using standard Vim directory names like 'plugin'
" {{{ plugin
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'

" Multiple Plug commands can be written in a single line using | separators

if has('nvim') 
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp' | Plug 'hrsh7th/cmp-buffer' | Plug 'hrsh7th/cmp-path' | Plug 'hrsh7th/cmp-cmdline' | Plug 'hrsh7th/nvim-cmp' 

Plug 'numToStr/Comment.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim' | Plug 'nvim-telescope/telescope-fzy-native.nvim' 
Plug 'kyazdani42/nvim-web-devicons'
Plug 'L3MON4D3/LuaSnip' | Plug 'rafamadriz/friendly-snippets'| Plug 'saadparwaiz1/cmp_luasnip'
Plug 'kyazdani42/nvim-tree.lua'
else
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets' 
endif

" On-demand loading
" colorscheme
"
Plug 'NLKNguyen/papercolor-theme'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()
" }}} plugin

set termguicolors
colorscheme PaperColor
set background=light
set ignorecase
set mouse+=a
set clipboard+=unnamedplus,unnamed
set ruler
set history=10000
set hidden

set sta
set noexpandtab
set softtabstop=8
set shiftwidth=8
set tabstop=8
set smartindent
set foldmethod=marker

" lua conf{{{ 
if has('nvim')
	luafile ~/.config/nvim/luaconf.lua
endif
" }}}
"
" gui stuff {{{
if has('gui_running') 
	set guifont=Monaco:h14
endif
" }}}
" keymap {{{
let mapleader="\<space>"
if !has('nvim')
	nmap <F6> :FZF<cr>
	nmap <F5> :!<cr>
	nmap <F3> :Rg<cr>
	nmap <leader>nt :NERDTreeToggle<cr>
else 
	nmap <leader>nt :NvimTreeToggle<cr>
endif
nmap <F2> :w<cr>
vnoremap > >gv
vnoremap < <gv

aug Comment
	au!
	au Filetype cpp set commentstring=\/\/\ %s
aug END

aug LC
	au!
	au Filetype cpp nmap <buffer> <F9> :!g++ -std=c++17 % && ./a.out < inp<cr>
	au Filetype cpp setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
	au Filetype go nmap <buffer> <F9> :!go run %<cr>
	au Filetype go nmap <buffer> <F10> :!go run % < inp<cr>
aug END

" }}}
