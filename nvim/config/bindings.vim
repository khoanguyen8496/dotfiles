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
" keybind maps
" tagbar map and config
nnoremap <leader>t :TagbarToggle<cr>
" nerdtree map and config
" nmap <leader>nt :NERDTreeToggle<cr>
" fzf binding
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>a :Ack
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
" nmap ; :
" vmap ; :
" map key for saving, nohl and make
nnoremap <leader>w :w<cr>
nnoremap <leader><space> :nohl<cr>
nnoremap <F4> :make!<cr>
" mapped key and :
vnoremap <silent> <TAB> >gv
vnoremap <silent> <S-TAB> <gv
" copy to end of line
nnoremap Y y$
" " copy to system clipboard
nnoremap gy "+y
" " copy whole file to system clipboard
nnoremap gY gg"+yG
" nnoremap ; :
" nnoremap : ;
tnoremap <ESC> <c-\><c-n>
