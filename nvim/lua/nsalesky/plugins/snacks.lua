return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    ---@type snacks.Config
    opts = {
      lazygit = {},
    },
    keys = {
      {
        "<leader>gb",
        function()
          Snacks.git.blame_line()
        end,
        desc = "Git blame line",
      },
      {
        "<leader>gs",
        function()
          Snacks.lazygit()
        end,
        desc = "Open lazygit",
      },
    },
  },
}
