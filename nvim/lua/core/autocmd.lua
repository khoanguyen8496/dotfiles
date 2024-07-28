local general_group = vim.api.nvim_create_augroup("KickstartGroup", {})

vim.api.nvim_create_autocmd("FocusGained", {
    desc = "Reload files from disk when we focus neovim",
    pattern = "*",
    command = "silent! checktime",
    group = general_group,
})
vim.api.nvim_create_autocmd("BufEnter", {
    desc = "Every time we enter an unmodified buffer, check if it changed on disk",
    pattern = "*",
    command = "if &buftype == '' && !&modified | exec 'checktime ' . expand('<abuf>') | endif",
    group = general_group,
})
vim.api.nvim_create_autocmd("BufReadPost", {
    desc = "Return to last edit position when opening files",
    pattern = "*",
    command =
    [[if line("'\"") > 0 && line("'\"") <= line("$") && expand('%:t') != 'COMMIT_EDITMSG' | exe "normal! g`\"" | endif]],
    group = general_group,
})
