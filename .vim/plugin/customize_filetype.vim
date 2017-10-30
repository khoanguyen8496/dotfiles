" load config for c++
"load config for java
function! JAVASET()
    setlocal shiftwidth=4
    setlocal softtabstop=4
    setlocal colorcolumn=80
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
    setlocal cinoptions=:0,l1,t0,g0,(s
    setlocal colorcolumn=80
    setlocal commentstring=//\ %s
    " call LoadCscope()
    nnoremap <buffer><silent> <f9> :w<cr>:!gcc -o2 -o %< %<cr>:!clear && ./%<<cr>
    nnoremap <buffer><silent> <f8> :w<cr>:!gcc -o2 -o %< % -g./<cr>
    nnoremap <buffer><silent> <f10> :w<cr> :!gdb %<<cr>
    " nnoremap <buffer> <leader>bc :!find . -iname '*.c' -o -iname '*.cpp' -o -iname '*.h' -o -iname '*.hpp' > cscope.files<CR>
  " \:!cscope -b -k<CR>
  " \:cs reset<CR>
endfunction

function! PYSET()
    setlocal shiftwidth=4
    setlocal foldmethod=indent
    setlocal softtabstop=4
    setlocal colorcolumn=81
    setlocal commentstring=#\ %s
    nnoremap <buffer><silent> <F9> :w<cr>:!python %<cr>
endfunction

function! JSSET()
    setlocal tabstop=2
    setlocal shiftwidth=2
    setlocal softtabstop=2
    setlocal colorcolumn=80
    setlocal commentstring=//\ %s
    setlocal updatetime=3000
    setlocal dictionary+=$HOME/.vim/dict/node.dict
    let g:tern_map_keys=1
    let g:tern_show_argument_hints='on_hold'
    let g:tern_show_signature_in_pum=1
endfunction

