" airline config
let w:airline_disabled = 0
let g:airline_powerline_fonts=0 " powerline option of vim-airline
let g:airline#extensions#ale#enabled = 1
" ultisnips config
let g:UltiSnipsExpandTrigger="<c-l>"
let g:UltiSnipsListSnippets="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetDirectories=["/home/nakhoa/.vim/plugged/vim-snippets/UltiSnips"]

" tmux with vim config 
let g:tmux_navigator_save_on_switch=2

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

" ale config
let g:ale_enabled = 0
let g:ale_sign_error = '>>'
let g:ale_sign_warning = '--'
" Write this in your vimrc file
let g:ale_lint_on_text_changed = 'never'
" if you don't want linters to run on opening a file
let g:ale_lint_on_enter = 0
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1

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

let g:SuperTabDefaultCompletionType = "context"

" ycm config
let g:ycm_max_num_candidates=30
let g:ycm_max_num_identifier_candidates=30
let g:ycm_warning_symbol='__'
let g:ycm_add_preview_to_completeopt=1
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_key_list_select_completion=['<c-n>', 'Down']
let g:ycm_key_list_previous_completion=['<c-p', 'Up']
let g:ycm_global_ycm_extra_conf='~/.vim/.ycm_extra_conf.py'
let g:ycm_disable_for_files_larger_than_kb=1024
