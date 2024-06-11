return {
  "nvim-tree/nvim-web-devicons",
  {
    "nvim-lualine/lualine.nvim",
    opts = require("nsalesky.configs.lualine"),
  },
  -- {
  --   "nvim-tree/nvim-tree.lua",
  --   opts = function()
  --     return require("nsalesky.configs.nvim-tree")
  --   end,
  --   cmd = { "NvimTreeToggle", "NvimTreeFocus" },
  --   keys = {
  --     {
  --       "<C-n>",
  --       "<cmd>NvimTreeToggle<CR>",
  --       desc = "Toggle nvimtree",
  --     },
  --     {
  --       "<leader>e",
  --       "<cmd>NvimTreeFocus<CR>",
  --       desc = "Focus nvimtree",
  --     },
  --   },
  -- },
}
