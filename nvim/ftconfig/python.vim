function! PYSET()
    setlocal expandtab
    setlocal shiftwidth=4
    setlocal foldmethod=indent
    setlocal softtabstop=4
    setlocal colorcolumn=81
    setlocal commentstring=#\ %s
    nnoremap <buffer><silent> <F9> :w<cr>:!python %<cr>
endfunction
autocmd FileType python call PYSET()
