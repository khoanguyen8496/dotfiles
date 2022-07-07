local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
	packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.g.mapleader=' '
require('packer').startup(function(use)
	-- My plugins here
	-- use 'foo1/bar1.nvim'
	-- use 'foo2/bar2.nvim'

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins

	use 'tpope/vim-surround'
	use 'tpope/vim-sensible'
	use 'tpope/vim-fugitive'
	use {
		'neovim/nvim-lspconfig', requires = {
			'williamboman/nvim-lsp-installer'
		}
	}
	use {
		'hrsh7th/nvim-cmp' ,
		requires = {
			'hrsh7th/cmp-nvim-lsp',
			'hrsh7th/cmp-buffer',
			'hrsh7th/cmp-path',
			'hrsh7th/cmp-cmdline',
			'saadparwaiz1/cmp_luasnip'
		}
	}
	use 'nvim-treesitter/nvim-treesitter'
	use {
		'nvim-telescope/telescope.nvim' ,
		requires = {
			'nvim-telescope/telescope-fzy-native.nvim',
			'nvim-telescope/telescope-ui-select.nvim',
			'nvim-lua/plenary.nvim'
		}
	}
	use {
		'L3MON4D3/LuaSnip',
		requies = 'rafamadriz/friendly-snippets'
	}
	use {
		'kyazdani42/nvim-tree.lua',
		requires = 'kyazdani42/nvim-web-devicons'
	}
	use 'NLKNguyen/papercolor-theme'
	use {
		'vim-airline/vim-airline',
		requires = {
			'vim-airline/vim-airline-themes'
		}
	}
	use 'numToStr/Comment.nvim'
	if packer_bootstrap then
		require('packer').sync()
	end
end)

vim.opt.termguicolors = true
vim.cmd('colorscheme PaperColor')
vim.opt.background=light
vim.opt.ignorecase = true
vim.opt.mouse:append('a')
vim.opt.clipboard:append({'unnamedplus','unnamed'})
vim.opt.ruler = true
vim.opt.history=10000
vim.opt.hidden = true

vim.opt.smartindent = true
vim.opt.expandtab = false
vim.opt.softtabstop=8
vim.opt.shiftwidth=8
vim.opt.tabstop=8
vim.cmd('set foldmethod=marker')
require('luaconf')
vim.keymap.set('n',  '<F2>', ':w<cr>')
vim.keymap.set('v', '>', '>gv') 
vim.keymap.set('v', '<',  '<gv')
