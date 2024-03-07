return {
  {
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300

      local wk = require("which-key")
      wk.setup({})
      wk.register({
        c = { name = "code" },
        d = { name = "debugging" },
        f = { name = "file" },
        g = { name = "git" },
        t = { name = "test" },
        s = { name = "settings" },
        b = { name = "buffer", s = { name = "sort" } },
        m = { name = "database" },
      }, { prefix = "<leader>" })
    end,
  },
  "nvim-tree/nvim-web-devicons",
  {
    "nvim-lualine/lualine.nvim",
    opts = require("nsalesky.configs.lualine"),
  },
  {
    "nvim-tree/nvim-tree.lua",
    opts = function()
      return require("nsalesky.configs.nvim-tree")
    end,
    cmd = { "NvimTreeToggle", "NvimTreeFocus" },
    keys = {
      {
        "<C-n>",
        "<cmd>NvimTreeToggle<CR>",
        desc = "Toggle nvimtree",
      },
      {
        "<leader>e",
        "<cmd>NvimTreeFocus<CR>",
        desc = "Focus nvimtree",
      },
    },
  },
}
