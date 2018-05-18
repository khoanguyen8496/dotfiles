set encoding=utf-8
set foldmethod=syntax
set path+=**,/usr/local/include
set nofoldenable
" backspace should work as we expect
set bs=2
set bg=dark
colorscheme base16-eighties
" if has('gui_running')
" 	colorscheme gruvbox
" 	let g:airline_theme='gruvbox'
" 	set bg=dark
" else
" 	colorscheme gruvbox
" 	set bg=dark
" 	let g:airline_theme='gruvbox'
" endif
"basic config
" true color vim
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
let c_syntax_for_h=0
set tabpagemax=100
set ruler         " show cursor position in bottom line
set nu            " show line number
" set rnu "show relative number
" set cursorline
" y and d put stuff into system clipboard (so that other apps can see it)
set clipboard=unnamed,unnamedplus
set nowrap
set textwidth=0
set ttimeout
set timeoutlen=500
set ttimeoutlen=100 " timeout when keypress belong to any combos

" Tab related stuffs
set shiftround    " when shifting non-aligned set of lines, align them to next tabstop
set smarttab
set expandtab

" list stuff
set listchars=tab:>-,trail:·
set list

" Searching
set ignorecase    " default should ignore case
set smartcase     " use case sensitive if I use uppercase

" autocomplete configuration
set complete+=t
" set complete+=k
set lazyredraw
set completeopt=menu,preview
set splitbelow
set dictionary+=/usr/share/dict/words
" 2 line for statusline
set cmdheight=2
" my own black and white status line
" set statusline=%<[%.30F]\ [%20.50(%h%w%m%r%)]%=\ 0x%B\ %l,%c%V\ %P
" set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L] 
let mapleader=','
let maplocalleader='\\'

" plugin config
" ack.vim config
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
  let grepprg = 'ag --vimgrep --nogroup --nocolor'
endif

