return {
  {
    "otavioschwanck/arrow.nvim",
    opts = {
      show_icons = true,
      leader_key = ";",
      separate_by_branch = true,
    },
    keys = {
      ";",
      "<cmd>Arrow<cr>",
      desc = "Open Arrow",
    },
  },
  {
    "mbbill/undotree",
    opts = {},
    keys = {
      {
        "<leader>u",
        ":UndotreeToggle<CR>",
        desc = "Undotree Toggle",
      },
    },
  },
  {
    "mikavilpas/yazi.nvim",
    event = "VeryLazy",
    opts = {
      open_for_directories = true,
      keymaps = {
        show_help = "<f1>",
      },
    },
    keys = {
      {
        "<leader>-",
        "<cmd>Yazi<cr>",
        desc = "Open Yazi at the current file",
      },
      {
        "<leader>fy",
        "<cmd>Yazi cwd<cr>",
        desc = "Open Yazi in the current working directory",
      },
    },
  },
  -- {
  --   "aserowy/tmux.nvim",
  --   lazy = false,
  --   opts = {
  --     copy_sync = {
  --       enable = false,
  --     },
  --     navigation = {
  --       cycle_navigation = true,
  --       enable_default_keybindings = true,
  --       persist_zoom = true,
  --     },
  --     resize = {
  --       enable_default_keybindings = true,
  --       resize_step_x = 1,
  --       resize_step_y = 1,
  --     },
  --   },
  --   config = function(_, opts)
  --     require("tmux").setup(opts)
  --   end,
  -- },
}
