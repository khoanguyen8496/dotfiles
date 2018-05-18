call plug#begin('~/.config/nvim/plugged')
" snippets
Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
" Plug 'Shougo/neosnippet'
" Plug 'Shougo/neosnippet-snippets'
" tree and tagbar
Plug 'majutsushi/tagbar'

" edit text
" Plug 'ervandew/supertab'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'godlygeek/tabular'

" tmux navigator
Plug 'christoomey/vim-tmux-navigator'

" colorschemes
"
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
Plug 'chriskempson/base16-vim'
Plug 'nanotech/jellybeans.vim'

" R util
Plug 'jalvesaq/nvim-r'

" buffers and files
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" js and node
Plug 'jelera/vim-javascript-syntax'
" go
" Plug 'fatih/vim-go'
" fuzzy search
Plug 'mileszs/ack.vim'

" linter
" Plug 'w0rp/ale'
Plug 'skywind3000/asyncrun.vim'

" web tag closin
Plug 'alvan/vim-closetag'

" autocomplete
" Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
Plug 'editorconfig/editorconfig-vim'


" autogen tags
" Plug 'ludovicchabant/vim-gutentags'

call plug#end()

" ultisnips config
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<c-l>"
let g:UltiSnipsListSnippets="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetDirectories=["/home/nakhoa/.config/nvim/plugged/vim-snippets/UltiSnips"]

" nvim-r config
command RStart let oldft=&ft | set ft=r | exe 'set ft='.oldft | let b:IsInRCode = function("DefaultIsInRCode") | normal <LocalLeader>rf
let R_notmuxconf=1
let R_nvim_wd=1
let R_in_buffer = 0
let R_applescript = 0
let R_tmux_split = 1
vmap <Space> <Plug>RDSendSelection
nmap <Space> <Plug>RDSendLine

" tagbar config
let g:tagbar_autoclose=1
" closetag config
" filenames like *.xml, *.html, *.xhtml, ...
" Then after you press <kbd>&gt;</kbd> in these files, this plugin will try to close the current tag.
let g:closetag_filenames = '*.html,*.xhtml,*.phtml'

" filenames like *.xml, *.xhtml, ...
" This will make the list of non closing tags self closing in the specified files.
let g:closetag_xhtml_filenames = '*.xhtml,*.jsx'

" integer value [0|1]
" This will make the list of non closing tags case sensitive (e.g. `<Link>` will be closed while `<link>` won't.)
let g:closetag_emptyTags_caseSensitive = 1

" Shortcut for closing tags, default is '>'
let g:closetag_shortcut = '>'

" Add > at current position without closing the current tag, default is ''
let g:closetag_close_shortcut = '<leader>>'
" vim-gitgutter
"
let g:gitgutter_grep_command = 'ag'

" let g:SuperTabDefaultCompletionType = "context"
nnoremap <leader>r :AsyncRun<space>
nnoremap <leader>aw :Ack -Q <cword> ./ <CR>
" gutentags config
" use universal ctags instead of exuberant ctags
let g:gutentags_ctags_executable='uctags'
let g:gutentags_ctags_executable_c='uctags --fields=+iaS --c++-kinds=+pd --language-force=C'
let g:gutentags_ctags_executable_cpp='uctags --fields=+iaS --c++-kinds=+p --language-force=C++'
let g:gutentags_generate_on_write=1
let g:gutentags_file_list_command = 'cat filelists'
" deoplete config
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
let s:snippetSupport=0
			" \ 'cpp': ['LanguageClient', 'buffer', 'tag'],
			" \ 'c': ['LanguageClient', 'buffer', 'tag'],
			" \ 'javascript': ['LanguageClient', 'buffer', 'tag'],
			" \ 'python': ['LanguageClient', 'buffer', 'tag'],
call deoplete#custom#option('sources', {
			\ '_': ['LanguageClient' ,'buffer', 'tag', 'file', 'dictionary'],
			\})

call deoplete#custom#source('_', 'matchers', ['matcher_full_fuzzy'])
" lightline config test
let g:lightline = {
			\ 'colorscheme': 'default',
			\ 'active': {
			\   'left': [ [ 'mode', 'paste' ],
			\             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
			\ },
			\ 'component_function': {
			\   'gitbranch': 'fugitive#head'
			\ },
			\ }
