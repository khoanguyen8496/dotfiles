" load config for c++
au BufRead,BufNewFile *.cpp,*.cc,*.cxx set filetype=cpp

function! CPPSET()
	nnoremap <buffer><silent> <F9> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I ./<cr>:!clear; time ./%<<cr>
	nnoremap <buffer><silent> <F8> :w<cr>:!g++ % -O2 -o %< -std=c++11 -I -g./<cr>
	nnoremap <buffer><silent> <F10> :w<cr> :!gdb %< <cr>
	setlocal commentstring=\/\/\ %s
	setlocal tags+=~/.vim/tags/cpp
endfunction

au Filetype cpp call CPPSET()
