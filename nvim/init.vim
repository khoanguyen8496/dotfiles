" - Avoid using standard Vim directory names like 'plugin'
" {{{ plugin
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align

Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'

" Multiple Plug commands can be written in a single line using | separators
Plug 'neovim/nvim-lspconfig' | Plug 'hrsh7th/cmp-nvim-lsp' | Plug 'hrsh7th/cmp-buffer' | Plug 'hrsh7th/cmp-path' | Plug 'hrsh7th/cmp-cmdline' | Plug 'hrsh7th/nvim-cmp'
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets' | Plug 'quangnguyen30192/cmp-nvim-ultisnips'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" colorscheme
"
Plug 'NLKNguyen/papercolor-theme'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
call plug#end()
" }}} plugin

set termguicolors
colorscheme PaperColor
set cursorline
set background=dark
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
luafile ~/.config/nvim/luaconf.lua
" }}}
"
" keymap {{{
let mapleader="\<space>"
nmap <F5> :!<cr>
nmap <F6> :FZF<cr>
nmap <F3> :Rg<cr>
nmap <F2> :w<cr>
nmap <leader>nt :NERDTreeToggle<cr>
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
aug END

" }}}
"
" highlight {{{
highlight link CompeDocumentation NormalFloat
" }}}
