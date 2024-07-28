return {
  -- {{{ Semantic code tools (treesitter)
  { -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-context",
    },
    config = function()
      require("nvim-treesitter.configs").setup({
        -- Add languages here that you want installed for treesitter
        ensure_installed = { "lua", "typescript", "tsx", "java", "go", "ruby", "bash", "markdown" },
        highlight = { enable = true },
        indent = {
          enable = true,
          -- Treesitter indentation for lua files has problems
          disable = { "lua" },
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "gnn", -- set to `false` to disable one of the mappings
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
          },
        },
      })
    end,
  },
  { -- Code outline
    "stevearc/aerial.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("aerial").setup()
      vim.keymap.set("n", "<leader>a", "<cmd>AerialToggle!<CR>", { desc = "[A]erial toggle" })
      vim.keymap.set({ "n", "v" }, "[s", "<cmd>AerialPrev<CR>", { desc = "Previous aerial symbol" })
      vim.keymap.set({ "n", "v" }, "]s", "<cmd>AerialNext<CR>", { desc = "Next aerial symbol" })
      vim.keymap.set({ "n", "v" }, "[S", "<cmd>AerialPrevUp<CR>", { desc = "Previous aerial parent symbol" })
      vim.keymap.set({ "n", "v" }, "]S", "<cmd>AerialNextUp<CR>", { desc = "Next aerial parent symbol" })
    end,
  },
  -- }}}

  -- {{{ Misc
  -- { "numToStr/Comment.nvim", config = true }, -- "gc" to comment visual regions/lines
  { "wellle/targets.vim" },                  -- Convenient text objects
  { "folke/which-key.nvim", config = true }, -- Visual shortcut reminder
  { "tpope/vim-sleuth" },                    -- Detect tabstop and shiftwidth automatically
  {                                          -- Git commands in nvim
    "tpope/vim-fugitive",
    dependencies = "tpope/vim-rhubarb",
  },
  { -- Make copy/paste work over ssh
    "ojroques/nvim-osc52",
    -- Only change the clipboard if we're in a SSH session
    enabled = os.getenv("SSH_CLIENT") ~= nil,
    config = function()
      local osc52 = require("osc52")
      -- Use the '+' register, so copy to clipboard would be `"+yy`
      local function copy(lines, _)
        osc52.copy(table.concat(lines, "\n"))
      end

      local function paste()
        return { vim.fn.split(vim.fn.getreg(""), "\n"), vim.fn.getregtype("") }
      end

      vim.g.clipboard = {
        name = "osc52",
        copy = { ["+"] = copy, ["*"] = copy },
        paste = { ["+"] = paste, ["*"] = paste },
      }
    end,
  },
  { -- Toggleable terminal windows
    "akinsho/toggleterm.nvim",
    opts = {
      open_mapping = [[<c-\>]],
    },
  },
  { -- Task runner
    "stevearc/overseer.nvim",
    opts = {
      templates = { "builtin", "bazel" },
      default_neotest = {
        { "on_complete_notify", on_change = true },
        "default",
      },
    },
    config = function(_, opts)
      require("overseer").setup(opts)
      vim.keymap.set("n", "<leader>ot", "<cmd>OverseerToggle<CR>", { desc = "[O]verseer [T]oggle" })
      vim.keymap.set("n", "<leader>or", "<cmd>OverseerRun<CR>", { desc = "[O]verseer [R]un" })
      vim.keymap.set("n", "<leader>oq", "<cmd>OverseerQuickAction<CR>", { desc = "[O]verseer [Q]uick action" })
      vim.keymap.set("n", "<leader>oa", "<cmd>OverseerTaskAction<CR>", { desc = "[O]verseer task [A]ction" })
    end,
  },
  -- }}}

  -- {{{ Test runner
  {
    "nvim-neotest/neotest",
    dependencies = {
      "haydenmeade/neotest-jest",
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "stevearc/overseer.nvim",
    },
    config = function()
      local neotest_jest = require("neotest-jest")
      local neotest = require("neotest")
      neotest.setup({
        adapters = {
          neotest_jest({
            cwd = neotest_jest.root,
          }),
          require("neotest-pay-test")(),
        },
        discovery = {
          enabled = false,
        },
        consumers = {
          overseer = require("neotest.consumers.overseer"),
        },
        icons = {
          passed = " ",
          running = " ",
          failed = " ",
          unknown = " ",
          running_animated = vim.tbl_map(function(s)
            return s .. " "
          end, { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }),
        },
        output = {
          open_on_run = false,
        },
      })
      vim.keymap.set("n", "<leader>tf", function()
        neotest.run.run({ vim.api.nvim_buf_get_name(0) })
      end, { desc = "[T]est [F]ile" })
      vim.keymap.set("n", "<leader>tn", function()
        neotest.run.run({})
      end, { desc = "[T]est [N]earest" })
      vim.keymap.set("n", "<leader>tl", neotest.run.run_last, { desc = "[T]est [L]ast" })
      vim.keymap.set("n", "<leader>ts", neotest.summary.toggle, { desc = "[T]est toggle [S]ummary" })
      vim.keymap.set("n", "<leader>to", function()
        neotest.output.open({ short = true })
      end, { desc = "[T]est [O]utput" })
    end,
  },
}
