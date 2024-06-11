return {
  {
    "echasnovski/mini.nvim",
    version = false,
    event = "VeryLazy",
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
      vim.keymap.set("n", "<leader>e", function() require("mini.files").open() end, { desc = "Open MiniFiles" })

      require("mini.hipatterns").setup({
        highlighters = {
          -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
          fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
          hack  = { pattern = "%f[%w]()HACK()%f[%W]",  group = "MiniHipatternsHack"  },
          todo  = { pattern = "%f[%w]()TODO()%f[%W]",  group = "MiniHipatternsTodo"  },
          note  = { pattern = "%f[%w]()NOTE()%f[%W]",  group = "MiniHipatternsNote"  },
        }
      })

      require("mini.diff").setup({})

      require("mini.surround").setup({})
    end,
  },
}
