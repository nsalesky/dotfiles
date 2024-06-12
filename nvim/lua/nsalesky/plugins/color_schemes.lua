return {
  {
    "catppuccin/nvim",
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        integrations = {
          cmp = true,
          gitsigns = true,
          telescope = true,
          nvimtree = true,
        },
      })

      vim.cmd.colorscheme("mini_cat")
    end,
  },
  -- "rebelot/kanagawa.nvim",
  -- "sainnhe/everforest",
  -- "folke/tokyonight.nvim",
}
