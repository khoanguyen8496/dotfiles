vim.opt.diffopt:append("linematch:60")
vim.o.background = 'light'
vim.o.clipboard = "unnamedplus"
vim.o.completeopt = "menu,menuone,popup,fuzzy"
-- vim.o.confirm = true
vim.o.cursorline = true
vim.o.expandtab = true
vim.o.foldcolumn = "0"
vim.o.foldenable = true
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldmethod = "expr"
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
if vim.fn.executable("rg") ~= 0 then
    vim.o.grepprg = "rg --vimgrep --no-heading --smart-case"
    vim.o.grepformat = "%f:%l:%c:%m,%f:%l:%m"
end
if vim.fn.executable("fzf") ~= 1 then
    vim.notify("fzf not found. brew install fzf", vim.log.levels.WARN)
end

-- Show when a line wraps
vim.o.showbreak = "↳ "

vim.o.inccommand = "split"
vim.o.ignorecase = true
vim.o.list = true
vim.opt.listchars = {
    tab = "▏ ",
    trail = "·",
    extends = "»",
    precedes = "«",
}
vim.o.mouse = "a"
vim.o.number = false
vim.o.pumheight = 10
vim.o.shiftround = true
vim.o.shiftwidth = 4
vim.o.showmode = true
vim.o.signcolumn = "yes"
vim.o.smartcase = true
vim.o.smartindent = true
vim.o.tabstop = 4
vim.o.termguicolors = true
vim.o.undofile = true
vim.o.undolevels = 10000
vim.o.updatetime = 200

vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.g.editorconfig = true

-- Shorten the default command timeout for better behavior with which-key.nvim
vim.o.timeoutlen = 500
-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0
-- When jumping to location via quickfix, use buffer in open windows
vim.o.switchbuf = "useopen,uselast"

vim.opt.shortmess:append("c") -- for nvim-cmp
vim.opt.shortmess:append("I") -- Hide the startup screen
vim.opt.shortmess:append("A") -- Ignore swap file messages
vim.opt.shortmess:append("a") -- Shorter message formats
-- Use 'g' flag by default with :s/foo/bar
vim.o.gdefault = true
-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = "yes"
-- Global statusline
vim.o.laststatus = 3
-- Enable break indent
vim.o.breakindent = true
