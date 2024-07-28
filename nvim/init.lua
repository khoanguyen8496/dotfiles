require("core.options")
require("core.statusline")
require("core.snippet")
require("core.autocmd")
require("core.diagnostics")
-- {{{ PLUGINS

-- bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

-- First require lazy to ensure the vim.uv shim is applied
require("lazy")
-- Monkey patch spawn command to work on remote devbox. Sorry, he wouldn't accept my PR T_T
local spawn = require("lazy.manage.process").spawn
require("lazy.manage.process").spawn = function(cmd, opts)
  opts = opts or {}
  opts.env = vim.tbl_extend("force", opts.env or {}, { GIT_CONFIG_NOSYSTEM = "1" })
  return spawn(cmd, opts)
end

require("lazy").setup({
  spec = {
    { import = "plugins/lazy" }
  }
})

vim.cmd [[packadd cfilter]]

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et fdm=marker
