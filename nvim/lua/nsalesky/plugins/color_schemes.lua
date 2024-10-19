return {
  {
    "catppuccin/nvim",
    config = function()
      require("catppuccin").setup({
        flavour = "macchiato",
        integrations = {
          cmp = true,
          gitsigns = true,
          telescope = true,
          nvimtree = true,
        },
      })
    end,
  },
  { "rose-pine/neovim", name = "rose-pine" },
  "rebelot/kanagawa.nvim",
  "sainnhe/everforest",
  "folke/tokyonight.nvim",
}
