" language client config
set hidden
let g:LanguageClient_serverCommands = {
			\ 'cpp': ['/usr/local/bin/cquery', '--log-file=/tmp/cppq.log'],
			\ 'c': ['/usr/local/bin/cquery', '--log-file=/tmp/cq.log'],
			\ 'python': ['/home/nakhoa/.local/bin/pyls', '--log-file=/tmp/pyls.log'],
			\ 'javascript': ['javascript-typescript-stdio', '--logfile', '/tmp/lsp.js.log']
			\ }

let g:LanguageClient_loadSettings = 1
" let $LANGUAGECLIENT_DEBUG=1
" let g:LanguageClient_loggingLevel='DEBUG'
" Use an absolute configuration path if you want system-wide settings
let g:LanguageClient_settingsPath = '/home/nakhoa/.config/nvim/settings.json'
let g:LanguageClient_autoStart = 1

set completefunc=LanguageClient#complete
set formatexpr=LanguageClient_textDocument_rangeFormatting()

nnoremap <silent> K    : call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd   : call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> gr   : call LanguageClient_textDocument_references()<CR>
nnoremap <silent> gs   : call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <silent> <F2> : call LanguageClient_textDocument_rename()<CR>
