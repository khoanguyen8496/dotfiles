" load config for c++
au BufRead,BufNewFile *.cpp,*.cc,*.cxx set filetype=cpp

function! CPPSET()
    setlocal tabstop=2
    setlocal shiftwidth=2
    setlocal softtabstop=2
    setlocal colorcolumn=81
    setlocal commentstring=//\ %s
    setlocal smartindent
    " call LoadCscope()
    nnoremap <buffer><silent> <F9> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I ./<cr>:!clear; time ./%<<cr>
    nnoremap <buffer><silent> <F8> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I -g./<cr>
    nnoremap <buffer><silent> <F10> :w<cr> :!gdb %< <cr>
    if executable('gtags')
        call GtagsCscope()
    endif
    " nnoremap <buffer> <leader>bc :!find . -iname '*.c' -o -iname '*.cpp' -o -iname '*.h' -o -iname '*.hpp' > cscope.files<CR>
  " \:!cscope -b -k<CR>
  " \:cs reset<CR>
endfunction

au Filetype cpp call CPPSET()
