" airline config
let w:airline_disabled = 0
let g:airline_powerline_fonts=0 " powerline option of vim-airline

" ultisnips config
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsListSnippets="<c-n>"
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


