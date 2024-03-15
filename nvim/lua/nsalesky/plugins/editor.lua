return {
  {
    "otavioschwanck/arrow.nvim",
    opts = {
      show_icons = true,
      leader_key = ";",
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
    "aserowy/tmux.nvim",
    lazy = false,
    opts = {
      copy_sync = {
        enable = false,
      },
      navigation = {
        cycle_navigation = true,
        enable_default_keybindings = true,
        persist_zoom = true,
      },
      resize = {
        enable_default_keybindings = true,
        resize_step_x = 1,
        resize_step_y = 1,
      },
    },
    config = function(_, opts)
      require("tmux").setup(opts)
    end,
  },
  {
    "tpope/vim-obsession",
  },
}
