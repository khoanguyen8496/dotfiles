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
	local line = unpack(vim.api.nvim_buf_get_lines(0, start_row, end_row+1, false))
	vim.cmd.grep(string.sub(line, start_col, end_col))
end
vim.g.mapleader=' '
vim.opt.ignorecase = true
vim.opt.mouse:append('a')
vim.opt.clipboard:append({'unnamedplus','unnamed'})
vim.opt.ruler = true
vim.opt.history=10000
vim.opt.hidden = true
vim.opt.showmatch=true
vim.opt.smartindent = true
vim.opt.expandtab = false
vim.opt.softtabstop=2
vim.opt.shiftwidth=2
vim.opt.tabstop=2

vim.cmd[[ set noswapfile ]]

vim.opt.termguicolors=true
vim.opt.background='dark'
vim.opt.cursorline=true
vim.opt.grepprg=[[rg --vimgrep]]
-- folding
vim.opt.foldmethod='indent'
vim.opt.foldlevel=1
vim.opt.foldnestmax=10
-- tpope status line
vim.opt.statusline=[[ [%n] %<%.99f %y%h%w%m%r%=%-14.(%l,%c%V%) %P ]]
vim.keymap.set('n',  '<F2>', ':w<cr>')
vim.keymap.set('v', '>', '>gv') 
vim.keymap.set('v', '<',  '<gv')
vim.keymap.set('n', 'sw', ':grep! <cword><cr>:copen<cr>')
vim.keymap.set('v', 'sw', ':lua Get_line_visual_selection()<cr>')
-- vim.keymap.set('v', 'sw', ':grep! '
require('packer').startup(function(use)
	-- use 'tpope/vim-surround'
	-- use 'tpope/vim-sensible'
	use {
		'hkupty/iron.nvim',
		config = function()
			local iron = require("iron.core")

			iron.setup {
				config = {
					-- Whether a repl should be discarded or not
					scratch_repl = true,
					-- Your repl definitions come here
					repl_definition = {
						sh = {
							-- Can be a table or a function that
							-- returns a table (see below)
							command = {"zsh"}
						},
						scm = {
							command = 'guile'
						}
					},
					-- How the repl window will be displayed
					-- See below for more information
					repl_open_cmd = 'split'
				},
				-- Iron doesn't set keymaps by default anymore.
				-- You can set them here or manually add keymaps to the functions in iron.core
				keymaps = {
					send_motion = "<space>rsc",
					visual_send = "<space>rsc",
					send_file = "<space>rsf",
					send_line = "<space>rsl",
					send_mark = "<space>rsm",
					mark_motion = "<space>rmc",
					mark_visual = "<space>rmc",
					remove_mark = "<space>rmd",
					cr = "<space>rs<cr>",
					interrupt = "<space>rs<space>",
					exit = "<space>rsq",
					clear = "<space>rcl",
				},
				-- If the highlight is on, you can change how it looks
				-- For the available options, check nvim_set_hl
				highlight = {
					italic = true
				},
				ignore_blank_lines = true, -- ignore blank lines when sending visual select lines
			}

			-- iron also has a list of commands, see :h iron-commands for all available commands
			-- vim.keymap.set('n', '<space>rs', '<cmd>IronRepl<cr>')
			-- vim.keymap.set('n', '<space>rr', '<cmd>IronRestart<cr>')
			-- vim.keymap.set('n', '<space>rf', '<cmd>IronFocus<cr>')
			-- vim.keymap.set('n', '<space>rh', '<cmd>IronHide<cr>')
		end
	}
	use {
		"rebelot/kanagawa.nvim",
		config=function ()
			vim.cmd [[colorscheme kanagawa]]
		end
	}
	use {
		'echasnovski/mini.nvim',
		config = function ()
			require('mini.basics').setup()
			require('mini.surround').setup()
			require('mini.comment').setup()
			require('mini.bracketed').setup()
		end
	}
	use 'tpope/vim-fugitive'
	use {
		'glacambre/firenvim',
		run = function() vim.fn['firenvim#install'](0) end 
	}
	use 'mfussenegger/nvim-jdtls'
	use { -- LSP Configuration & Plugins
		'neovim/nvim-lspconfig',
		requires = {
			-- Automatically install LSPs to stdpath for neovim
			'williamboman/mason.nvim',
			'williamboman/mason-lspconfig.nvim',

			-- Useful status updates for LSP
			'j-hui/fidget.nvim',

			-- Additional lua configuration, makes nvim stuff amazing
			'folke/neodev.nvim',
		},
		config = function()
			-- LSP settings.
			--  This function gets run when an LSP connects to a particular buffer.
			local on_attach = function(_, bufnr)
				-- NOTE: Remember that lua is a real programming language, and as such it is possible
				-- to define small helper and utility functions so you don't have to repeat yourself
				-- many times.
				--
				-- In this case, we create a function that lets us more easily define mappings specific
				-- for LSP related items. It sets the mode, buffer and description for us each time.
				local nmap = function(keys, func, desc)
					if desc then
						desc = 'LSP: ' .. desc
					end

					vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
				end

				vim.cmd([[
	nnoremap <buffer> K <cmd>lua vim.lsp.buf.hover()<cr>
	nnoremap <buffer> crq <cmd>lua vim.diagnostic.setqflist()<cr>
	nnoremap <buffer> crr <cmd>lua vim.lsp.buf.code_action()<cr>
	nnoremap <buffer> crn <cmd>lua vim.lsp.buf.rename()<cr>
	nnoremap <buffer> gO <cmd>lua vim.lsp.buf.document_symbol()<cr>
	nnoremap <buffer> gd <cmd>lua vim.lsp.buf.definition()<cr>
	nnoremap <buffer> gr <cmd>lua vim.lsp.buf.references()<cr>
	nnoremap <buffer> gi <cmd>lua vim.lsp.buf.implementation()<cr>
	]])
				-- Create a command `:Format` local to the LSP buffer
				vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
					vim.lsp.buf.format()
					end, { desc = 'Format current buffer with LSP' })
			end

			-- Enable the following language servers
			--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
			--
			--  Add any additional override configuration in the following tables. They will be passed to
			--  the `settings` field of the server config. You must look up that documentation yourself.
			local servers = {
				clangd = {},
				pyright = {},
				rust_analyzer = {},
				tsserver = {},
			}

			-- Setup neovim lua configuration
			require('neodev').setup()
			-- Setup mason so it can manage external tooling
			require('mason').setup()
			-- Ensure the servers above are installed
			local mason_lspconfig = require 'mason-lspconfig'

			mason_lspconfig.setup {
				ensure_installed = vim.tbl_keys(servers),
			}

			mason_lspconfig.setup_handlers {
				function(server_name)
					require('lspconfig')[server_name].setup {
						capabilities = capabilities,
						on_attach = on_attach,
						settings = servers[server_name],
					}
				end,
			}

			-- Turn on lsp status information
			require('fidget').setup()

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
	use { -- Autocompletion
		'hrsh7th/nvim-cmp',
		requires = { 'hrsh7th/cmp-nvim-lsp', 'L3MON4D3/LuaSnip', 'saadparwaiz1/cmp_luasnip' },
		config = function ()
			-- nvim-cmp setup
			local cmp = require 'cmp'
			local luasnip = require 'luasnip'

			cmp.setup {
				snippet = {
					expand = function(args)
						luasnip.lsp_expand(args.body)
					end,
				},
				mapping = cmp.mapping.preset.insert {
					['<C-d>'] = cmp.mapping.scroll_docs(-4),
					['<C-f>'] = cmp.mapping.scroll_docs(4),
					['<C-Space>'] = cmp.mapping.complete(),
					['<CR>'] = cmp.mapping.confirm {
						behavior = cmp.ConfirmBehavior.Replace,
						select = true,
					},
					['<Tab>'] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_next_item()
						elseif luasnip.expand_or_jumpable() then
							luasnip.expand_or_jump()
						else
							fallback()
						end
						end, { 'i', 's' }),
					['<S-Tab>'] = cmp.mapping(function(fallback)
						if cmp.visible() then
							cmp.select_prev_item()
						elseif luasnip.jumpable(-1) then
							luasnip.jump(-1)
						else
							fallback()
						end
						end, { 'i', 's' }),
				},
				sources = {
					{ name = 'nvim_lsp' },
					{ name = 'luasnip' },
				},
			}
			-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
		end 
	}

	use {
		'L3MON4D3/LuaSnip',
		requires = 'rafamadriz/friendly-snippets',
		config = function  ()
			local luasnip = require('luasnip')
			require("luasnip.loaders.from_vscode").load()
		end
	}
	use {
		'junegunn/fzf.vim',
		requires = 'junegunn/fzf',
		config = function() 
			vim.cmd [[ let g:fzf_preview_window = [] ]]
			vim.keymap.set('n', '<leader>sf', ':Files<cr>')
			vim.keymap.set('n', '<leader>rg', ':Rg<cr>')
		end
	}

	if packer_bootstrap then
		require('packer').sync()
	end
end)
