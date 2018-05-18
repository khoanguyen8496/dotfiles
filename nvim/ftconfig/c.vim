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
autocmd FileType c      call CSET()
