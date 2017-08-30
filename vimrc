call plug#begin('~/.vim/plugged')
Plug 'honza/vim-snippets'
Plug 'jnurmine/Zenburn'
Plug 'SirVer/ultisnips'
Plug 'ervandew/supertab'
Plug 'majutsushi/tagbar'
Plug 'scrooloose/nerdtree' 
Plug 'kien/ctrlp.vim'
" Plug 'wincent/command-t'
Plug 'NLKNguyen/papercolor-theme'
Plug 'sjl/badwolf'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'shougo/denite.nvim'
Plug 'valloric/youcompleteme'
Plug 'christoomey/vim-tmux-navigator'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'nanotech/jellybeans.vim'
Plug 'neomake/neomake'
Plug 'morhetz/gruvbox'
Plug 'jalvesaq/nvim-r'
Plug 'junegunn/seoul256.vim'
Plug 'beigebrucewayne/Turtles'
call plug#end()

set nocompatible
so ~/.vim/scripts/cscope_maps.vim
" so ~/.vim/scripts/google_python_style.vim
set encoding=utf-8
set foldmethod=syntax
" set term=screen
set path+=**
set nofoldenable
set bs=2          " backspace should work as we expect
set bg=dark
if has('gui_running')
    colorscheme badwolf
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
let c_syntax_for_h=1
set tabpagemax=100
set autoindent
set autowrite
set autoread
set wildmenu
set ruler         " show cursor position in bottom line
set nu            " show line number
set rnu "show relative number
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
set shiftwidth=4
set expandtab
set softtabstop=4
set shiftround    " when shifting non-aligned set of lines, align them to next tabstop
set smarttab

set listchars=tab:>-,trail:.
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
set tags=./tags,tags,~/.vim/tags/cpp
" 2 line for statusline
set cmdheight=2

let mapleader=','
let maplocalleader=','

" colorscheme jellybeans
" colorscheme PaperColor
" colorscheme seoul256
" colorscheme badwolf
colorscheme gruvbox
let g:airline_theme='gruvbox'
" let g:seoul256_background=235
" set t_Co=256

" youcompleteme config
let g:ycm_global_ycm_extra_conf='~/.vim/.ycm_extra_conf.py'
let g:ycm_autoclose_preview_window_after_completion = 1		 " Auto close preview tab
let g:ycm_goto_buffer_command = 'vertical-split'						" Goto definition in new split
nnoremap <leader>jd :YcmCompleter GoTo<CR>
let g:ycm_key_list_select_completion = ['<TAB>', '<Down>']
let g:ycm_collect_identifiers_from_tags_files = 0					 " Let YCM read tags from Ctags file
let g:ycm_use_ultisnips_completer = 1											 " Default 1, just ensure
let g:ycm_seed_identifiers_with_syntax = 1									" Completion for programming language's keyword
let g:ycm_complete_in_comments = 1													" Completion in comments
let g:ycm_complete_in_strings = 1													 " Completion in string
let g:ycm_key_list_previous_completion=['<Up>']
" airline config
let w:airline_disabled = 0
let g:airline_powerline_fonts=0 " powerline option of vim-airline

" ultisnips config
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsListSnippets="<c-s-n>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"
let g:UltiSnipsSnippetDirectories=["/home/nakhoa/.vim/plugged/vim-snippets/UltiSnips"]

" tmux with vim config 
let g:tmux_navigator_save_on_switch=2

" neomake for c++
let g:neomake_cpp_enabled_makers = ['clang', 'gcc']
let g:neomake_cpp_gcc_args = ['-std=c++11', '-Wall', '-Wextra', '-O2']
let g:neomake_cpp_clang_args = ['-std=c++11', '-Wall', '-Wextra', '-O2', '-lstdc++']
"neomake for python
let g:neomake_python_enabled_makers=['python']
" nvim-r config
" command RStart let oldft=&ft | set ft=r | exe 'set ft='.oldft | let b:IsInRCode = function("DefaultIsInRCode") | normal <LocalLeader>rf
let R_notmuxconf=1
let R_nvim_wd=1
let R_in_buffer = 0
let R_applescript = 0
let R_tmux_split = 1
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine

" tagbar config
let g:tagbar_autoclose=1

" keybind maps
" tagbar map and config
nmap <leader>t :TagbarToggle<cr>
" nerdtree map and config
nmap <leader>nt :NERDTreeToggle<cr>
" map key for fast split-switching
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l
nmap <C-h> <C-w>h
nnoremap <C-n> gt
nmap <c-m> gT
nmap <leader>w :w<cr>
nmap <leader><space> :nohl<cr>
nmap <leader>m :make<cr>
inoremap jj <ESC>
inoremap jk <ESC>
" open and close syntastic status bar
nmap <leader>er :lopen<cr>
nmap <leader>el :lclose<cr>
nnoremap <leader>sh :shell<cr>
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
" tag builder for c++
nnoremap <leader>bt :!$HOME/.vim/scripts/ctags_deps.sh $(cat fileslist)<cr>

" cursor config
" 
" load config for c++
function! CPPSET()
    setlocal shiftwidth=4
    setlocal softtabstop=4
    setlocal colorcolumn=81
    setlocal commentstring=//\ %s
    call LoadCscope()
    nnoremap <buffer><silent> <F9> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I ./<cr>:!clear; time ./%<<cr>
    nnoremap <buffer><silent> <F8> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I -g./<cr>
    nnoremap <buffer><silent> <F10> :w<cr> :!gdb %< <cr>
    nnoremap <buffer> <leader>bc :!find . -iname '*.c' -o -iname '*.cpp' -o -iname '*.h' -o -iname '*.hpp' > cscope.files<CR>
  \:!cscope -b -k<CR>
  \:cs reset<CR>
endfunction

"load config for java
function! JAVASET()
    setlocal shiftwidth=4
    setlocal softtabstop=4
    setlocal colorcolumn=81
    nnoremap <buffer> <F8> :!javac %<cr>
    nnoremap <buffer> <F9> :!javac %<cr>:!clear;java %< %<cr>
endfunction

" load config for ansi C
function! CSET() 
    setlocal tabstop=8
    setlocal shiftwidth=8
    setlocal softtabstop=8
    setlocal textwidth=80
    setlocal noexpandtab

    setlocal cindent
    setlocal cinoptions=:0,l1,t0,g0,(s
    setlocal colorcolumn=81
    setlocal commentstring=//\ %s
    call LoadCscope()
    nnoremap <buffer><silent> <f9> :w<cr>:!gcc -o2 -o %< %<cr>:!clear && ./%<<cr>
    nnoremap <buffer><silent> <f8> :w<cr>:!gcc -o2 -o %< % -g./<cr>
    nnoremap <buffer><silent> <f10> :w<cr> :!gdb %<<cr>
    nnoremap <buffer> <leader>bc :!find . -iname '*.c' -o -iname '*.cpp' -o -iname '*.h' -o -iname '*.hpp' > cscope.files<CR>
  \:!cscope -b -k<CR>
  \:cs reset<CR>
endfunction

function! PYSET()
    setlocal shiftwidth=4
    setlocal foldmethod=indent
    setlocal softtabstop=4
    setlocal colorcolumn=81
    setlocal commentstring=#\ %s
    nnoremap <buffer><silent> <F9> :w<cr>:!python %<cr>
endfunction

function! LoadCscope()
  let db = findfile("cscope.out", ".;")
  if (!empty(db))
    let path = strpart(db, -1, match(db, "/cscope.out$"))
    set nocscopeverbose " suppress 'duplicate connection' error
    exe "cs add " . db . " " . path
    set cscopeverbose
  endif
endfunction
" highlight LineNr ctermfg=Gray ctermbg=Black
" " huu nguyen config for seoul256 color
" highlight CursorLineNr ctermfg=Blue ctermbg=Black
" highlight TabLineSel ctermfg=White ctermbg=Black
" highlight TabLine ctermfg=Black ctermbg=Gray
" highlight Search ctermfg=Black ctermbg=Yellow
" highlight VertSplit ctermfg=236 ctermbg=236
" highlight ColorColumn ctermbg=5
" highlight CursorLine ctermbg=237
" highlight SignifySignAdd ctermfg=Green ctermbg=Black
" highlight SignifySignChange ctermfg=Yellow ctermbg=Black
" highlight SignifySignDelete ctermfg=Red ctermbg=Black
" highlight link SignifySignDeleteFirstLine SignifySignDelete
" highlight link SignifySignChangeDelete SignifySignChange
" highlight ExtraWhitespace ctermbg=Red

au! BufWritePost *.cpp Neomake " after writing cpp file make that file againjk,w;w
autocmd FileType cpp    call CPPSET()
autocmd FileType java   call JAVASET()
autocmd FileType c      call CSET()
autocmd FileType python call PYSET()
