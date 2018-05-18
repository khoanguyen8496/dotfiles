function! JAVASET()
    setlocal tabstop=4
    setlocal shiftwidth=4
    setlocal softtabstop=4
    setlocal colorcolumn=100
    setlocal omnifunc=javacomplete#Complete
    nmap <buffer> <F4> <Plug>(JavaComplete-Imports-AddSmart)
    imap <buffer> <F4> <Plug>(JavaComplete-Imports-AddSmart)
    nmap <buffer> <F5> <Plug>(JavaComplete-Imports-Add)
    imap <buffer> <F5> <Plug>(JavaComplete-Imports-Add)
    nmap <buffer> <F6> <Plug>(JavaComplete-Imports-AddMissing)
    imap <buffer> <F6> <Plug>(JavaComplete-Imports-AddMissing)
    nmap <buffer> <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
    imap <buffer> <F7> <Plug>(JavaComplete-Imports-RemoveUnused)
    nmap <buffer> <F8> <Plug>(JavaComplete-Imports-SortImports)
    imap <buffer> <F8> <Plug>(JavaComplete-Imports-SortImports)
    nmap <buffer> <leader>jI <Plug>(JavaComplete-Imports-AddMissing)
    nmap <buffer> <leader>jR <Plug>(JavaComplete-Imports-RemoveUnused)
    nmap <buffer> <leader>ji <Plug>(JavaComplete-Imports-AddSmart)
    nmap <buffer> <leader>jii <Plug>(JavaComplete-Imports-Add)
    nmap <buffer> <Leader>jis <Plug>(JavaComplete-Imports-SortImports)
    nmap <buffer> <leader>jM <Plug>(JavaComplete-Generate-AbstractMethods)
    nmap <buffer> <leader>jA <Plug>(JavaComplete-Generate-Accessors)
    nmap <buffer> <leader>js <Plug>(JavaComplete-Generate-AccessorSetter)
    nmap <buffer> <leader>jg <Plug>(JavaComplete-Generate-AccessorGetter)
    nmap <buffer> <leader>ja <Plug>(JavaComplete-Generate-AccessorSetterGetter)
    nmap <buffer> <leader>jts <Plug>(JavaComplete-Generate-ToString)
    nmap <buffer> <leader>jeq <Plug>(JavaComplete-Generate-EqualsAndHashCode)
    nmap <buffer> <leader>jc <Plug>(JavaComplete-Generate-Constructor)
    nmap <buffer> <leader>jcc <Plug>(JavaComplete-Generate-DefaultConstructor)
    vmap <buffer> <leader>js <Plug>(JavaComplete-Generate-AccessorSetter)
    vmap <buffer> <leader>jg <Plug>(JavaComplete-Generate-AccessorGetter)
    vmap <buffer> <leader>ja <Plug>(JavaComplete-Generate-AccessorSetterGetter)
endfunction

autocmd FileType java   call JAVASET()
