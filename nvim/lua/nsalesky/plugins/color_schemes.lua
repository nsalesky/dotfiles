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

      vim.cmd.colorscheme("catppuccin-macchiato")
    end,
  },
  -- "rebelot/kanagawa.nvim",
  -- "sainnhe/everforest",
  -- "folke/tokyonight.nvim",
}
