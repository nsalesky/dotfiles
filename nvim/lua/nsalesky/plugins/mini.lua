return {
  {
    "echasnovski/mini.nvim",
    version = false,
    dependencies = {
      { "nvim-treesitter/nvim-treesitter-textobjects" },
    },
    config = function()
      -- mini.ai : "around" and "inside" selections
      local gen_spec = require("mini.ai").gen_spec
      require("mini.ai").setup({
        custom_textobjects = {
          -- Function definition
          F = gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
        }
      })

      require("mini.pairs").setup()

      require("mini.files").setup()
    end,
  }
}
