return {
  {
    "stevearc/oil.nvim",
    config = function()
      require 'oil'.setup({
        default_file_explorer = true,
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
    end
  },
  -- {{{ UI plugins
  { "kyazdani42/nvim-web-devicons" }, -- Snazzy file icons (requires patched font)
  {                                   -- mini.nvim
    'echasnovski/mini.nvim',
    config = function()
      require 'mini.surround'.setup()
    end,
  },
  { -- Theme ported from VS Code
    "folke/tokyonight.nvim",
    priority = 1000,
    config = function()
      -- vim.cmd.colorscheme({ args = { "tokyonight-day" } })
      -- vim.cmd [[set background=light]]
    end,
  },

  -- {{{ Fuzzy finders
  { -- Fuzzy finder for files
    "ibhagwan/fzf-lua",
    opts = {
      files = {
        previewer = false,
      },
      -- This is required to support older version of fzf on remote devboxes
      fzf_opts = { ["--border"] = false },
      -- These settings reduce lag from slow git operations
      global_git_icons = false,
      git = {
        files = {
          previewer = false,
        },
      },
    },
    config = function(_, opts)
      require("fzf-lua").setup(opts)
      vim.keymap.set("n", "<leader>ff", require("fzf-lua").files, { desc = "[F]ind [F]iles" })
    end,
  },
}
