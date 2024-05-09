return {
  {
    "GCBallesteros/NotebookNavigator.nvim",
    keys = {
      {
        "]h",
        function()
          require("notebook-navigator").move_cell("d")
        end,
      },
      {
        "[h",
        function()
          require("notebook-navigator").move_cell("u")
        end,
      },
    },
    dependencies = {
      {
        "benlubas/molten-nvim", -- repl provider
        version = "^1.0.0",
        build = ":UpdateRemotePlugins",
        init = function() end
      },
      {
        "vhyrro/luarocks.nvim",
        priority = 1001,
        opts = {
          rocks = { "magick" }
        }
      },
      {
        "3rd/image.nvim",
        opts = {
          backend = "kitty",
          integrations = {},
          max_width = 100,
          max_height = 12,
          -- max_height_window_percentage = math.huge, -- this is necessary for a good experience
          -- max_width_window_percentage = math.huge,
          window_overlap_clear_enabled = true,
          window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
        }
      },
      "anuvyklack/hydra.nvim",
    },
    event = "VeryLazy",
    opts = {
      activate_hydra_keys = "<leader>h",
      repl_provider = "molten"
    },
    config = true
  },
  {
    "GCBallesteros/jupytext.nvim",
    config = true,
    lazy = false,
  }
}
