-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path }
  vim.cmd [[packadd packer.nvim]]
end

vim.cmd [[ packadd cfilter ]]
-- get from nvim-treesitter to get position from visual selection
local function visual_selection_range()
  local _, csrow, cscol, _ = unpack(vim.fn.getpos("'<"))
  local _, cerow, cecol, _ = unpack(vim.fn.getpos("'>"))
  if csrow < cerow or (csrow == cerow and cscol <= cecol) then
    return csrow - 1, cscol - 1, cerow - 1, cecol
  else
    return cerow - 1, cecol - 1, csrow - 1, cscol
  end
end
function Get_line_visual_selection()
  local start_row, start_col, end_row, end_col = visual_selection_range()
  if start_row ~= end_row then
    error('grep visual needs to be in the same line')
  end
  local line = unpack(vim.api.nvim_buf_get_lines(0, start_row, end_row + 1, false))
  vim.cmd.grep(string.sub(line, start_col, end_col))
end

vim.g.mapleader = ' '
vim.opt.ignorecase = true
vim.opt.mouse:append('a')
vim.opt.clipboard:append({ 'unnamedplus', 'unnamed' })
vim.opt.ruler = true
vim.opt.history = 10000
vim.opt.hidden = true
vim.opt.showmatch = true
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2

vim.cmd [[ set noswapfile ]]

vim.opt.termguicolors = true
vim.opt.background = 'dark'
vim.opt.cursorline = true
vim.opt.grepprg = [[rg --vimgrep]]
-- folding
vim.opt.foldmethod = 'indent'
vim.opt.foldlevel = 1
vim.opt.foldnestmax = 10
vim.opt.foldlevel = 999
-- tpope status line
-- vim.opt.statusline = [[ [%n] %<%.99f %y%h%w%m%r%=%-14.(%l,%c%V%) %P ]]
vim.keymap.set('n', '<F2>', ':w<cr>')
vim.keymap.set('v', '>', '>gv')
vim.keymap.set('v', '<', '<gv')
vim.keymap.set('n', 'sw', ':grep! <cword><cr>:copen<cr>')
vim.keymap.set('v', 'sw', ':lua Get_line_visual_selection()<cr>')
-- vim.keymap.set('v', 'sw', ':grep! '
require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'
  use {
    "rebelot/kanagawa.nvim",
    config = function()
      vim.cmd [[autocmd VimEnter * colorscheme kanagawa]]
    end
  }
  use {
    'echasnovski/mini.nvim',
    config = function()
      require('mini.surround').setup()
      require('mini.comment').setup()
      require('mini.bracketed').setup()
      require 'mini.statusline'.setup({})
      require 'mini.completion'.setup({})
    end
  }
  use 'tpope/vim-rsi'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-dispatch'
  use 'tpope/vim-projectionist'
  use {
    'glacambre/firenvim',
    run = function() vim.fn['firenvim#install'](0) end
  }
  use 'mfussenegger/nvim-jdtls'
  use { -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    config = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
          vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
          vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts)
          vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
          vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
          vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
          vim.keymap.set('n', '<space>f', function()
            vim.lsp.buf.format { async = true }
          end, opts)
        end,
      })

      -- Enable the following language servers
      --  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
      --
      --  Add any additional override configuration in the following tables. They will be passed to
      --  the `settings` field of the server config. You must look up that documentation yourself.
      local lspconfig = require 'lspconfig'
      lspconfig.lua_ls.setup {}
      lspconfig.metals.setup {}
      lspconfig.clangd.setup {}
      lspconfig.tsserver.setup {}
    end
  }

  use { -- Highlight, edit, and navigate code
    'nvim-treesitter/nvim-treesitter',
    run = function()
      pcall(require('nvim-treesitter.install').update { with_sync = true })
    end,
    config = function()
      require('nvim-treesitter.configs').setup {
        -- Add languages to be installed here that you want installed for treesitter
        ensure_installed = { 'c', 'cpp', 'go', 'lua', 'python', 'rust', 'typescript', 'help', 'vim' },

        highlight = { enable = true },
        indent = { enable = true, disable = { 'python' } },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = '<c-space>',
            node_incremental = '<c-space>',
            scope_incremental = '<c-s>',
            node_decremental = '<c-backspace>',
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ['aa'] = '@parameter.outer',
              ['ia'] = '@parameter.inner',
              ['af'] = '@function.outer',
              ['if'] = '@function.inner',
              ['ac'] = '@class.outer',
              ['ic'] = '@class.inner',
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              [']m'] = '@function.outer',
              [']]'] = '@class.outer',
            },
            goto_next_end = {
              [']M'] = '@function.outer',
              [']['] = '@class.outer',
            },
            goto_previous_start = {
              ['[m'] = '@function.outer',
              ['[['] = '@class.outer',
            },
            goto_previous_end = {
              ['[M'] = '@function.outer',
              ['[]'] = '@class.outer',
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ['<leader>a'] = '@parameter.inner',
            },
            swap_previous = {
              ['<leader>A'] = '@parameter.inner',
            },
          },
        },
      }
    end
  }

  use { -- Additional text objects via treesitter
    'nvim-treesitter/nvim-treesitter-textobjects',
    after = 'nvim-treesitter',
  }
  -- use { -- Autocompletion
  --   'hrsh7th/nvim-cmp',
  --   requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
  --   config = function()
  --     -- nvim-cmp setup
  --     local cmp = require 'cmp'
  --     local luasnip = require 'luasnip'
  --
  --     cmp.setup {
  --       snippet = {
  --         expand = function(args)
  --           luasnip.lsp_expand(args.body)
  --         end,
  --       },
  --       mapping = cmp.mapping.preset.insert {
  --         ['<C-d>'] = cmp.mapping.scroll_docs(-4),
  --         ['<C-f>'] = cmp.mapping.scroll_docs(4),
  --         ['<C-Space>'] = cmp.mapping.complete(),
  --         ['<CR>'] = cmp.mapping.confirm {
  --           behavior = cmp.ConfirmBehavior.Replace,
  --           select = true,
  --         },
  --         ['<Tab>'] = cmp.mapping(function(fallback)
  --           if cmp.visible() then
  --             cmp.select_next_item()
  --           elseif luasnip.expand_or_jumpable() then
  --             luasnip.expand_or_jump()
  --           else
  --             fallback()
  --           end
  --         end, { 'i', 's' }),
  --         ['<S-Tab>'] = cmp.mapping(function(fallback)
  --           if cmp.visible() then
  --             cmp.select_prev_item()
  --           elseif luasnip.jumpable(-1) then
  --             luasnip.jump(-1)
  --           else
  --             fallback()
  --           end
  --         end, { 'i', 's' }),
  --       },
  --       sources = {
  --         { name = 'nvim_lsp' },
  --         { name = 'luasnip' },
  --       },
  --     }
  --     -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
  --     local capabilities = vim.lsp.protocol.make_client_capabilities()
  --     capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
  --   end
  -- }
  --
  use {
    'L3MON4D3/LuaSnip',
    requires = 'rafamadriz/friendly-snippets',
    config = function()
      local luasnip = require('luasnip')
      require("luasnip.loaders.from_vscode").load()

      vim.cmd [[
  imap <silent><expr> <C-k> '<Plug>luasnip-expand-or-jump'
  " -1 for jumping backwards.
  inoremap <silent> <C-j> <cmd>lua require'luasnip'.jump(-1)<Cr>
  inoremap <silent> <C-l> <cmd>lua require('luasnip').jump(1)<Cr>

  snoremap <silent> <C-j> <cmd>lua require'luasnip'.jump(-1)<Cr>
  snoremap <silent> <C-l> <cmd>lua require('luasnip').jump(1)<Cr>

  " For changing choices in choiceNodes (not strictly necessary for a basic setup).
  " imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
  " smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
  ]]
    end
  }
  use {
    'junegunn/fzf',
    run = './install --bin'
  }
  use {
    'junegunn/fzf.vim',
    config = function()
      vim.cmd [[
  nnoremap <leader>ss :Rg<cr>
  nnoremap <leader>sf :Files<cr>
  nnoremap <leader>sb :Buffers<cr>
  ]]
    end
  }

  if is_bootstrap then
    require('packer').sync()
  end
end)
