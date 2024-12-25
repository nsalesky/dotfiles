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
        },
      })

      require("mini.pairs").setup()

      -- require("mini.files").setup()
      -- vim.keymap.set("n", "<leader>e", function()
      --   require("mini.files").open()
      -- end, { desc = "Open MiniFiles" })

      require("mini.hipatterns").setup({
        highlighters = {
          -- Highlight standalone 'FIXME', 'HACK', 'TODO', 'NOTE'
          fixme = { pattern = "%f[%w]()FIXME()%f[%W]", group = "MiniHipatternsFixme" },
          hack = { pattern = "%f[%w]()HACK()%f[%W]", group = "MiniHipatternsHack" },
          todo = { pattern = "%f[%w]()TODO()%f[%W]", group = "MiniHipatternsTodo" },
          note = { pattern = "%f[%w]()NOTE()%f[%W]", group = "MiniHipatternsNote" },
        },
      })

      require("mini.diff").setup({})

      require("mini.git").setup({})

      require("mini.surround").setup({})

      -- require("mini.completion").setup({
      --   lsp_completion = {
      --     source_func = "omnifunc",
      --     auto_setup = false,
      --   },
      -- })

      -- local mini_extra = require("mini.extra")
      -- mini_extra.setup({})

      -- local mini_pick = require("mini.pick")
      -- mini_pick.setup({})
      -- vim.keymap.set("n", "<C-p>", function()
      --   mini_pick.builtin.files({ tool = "git" })
      -- end)
      -- vim.keymap.set("n", "<leader>fs", function()
      --   mini_pick.builtin.grep_live()
      -- end, { desc = "Search" })
      -- vim.keymap.set("n", "<leader>fb", function()
      --   mini_pick.builtin.buffers()
      -- end, { desc = "Buffers" })
      -- vim.keymap.set("n", "<leader>fh", function()
      --   mini_extra.pickers.hl_groups()
      -- end, { desc = "HL Groups" })

      -- require("mini.statusline").setup({})

      -- local mini_status = require("mini.statusline")
      -- mini_status.setup({
      --   content = {
      --     active = function()
      --       local statusline = require("nsalesky.configs.statusline")
      --       statusline.setup()
      --
      --       local mode, mode_hl = mini_status.section_mode({ trunc_width = 120 })
      --       local git = mini_status.section_git({ trunc_width = 40 })
      --       local diff = statusline.section_diff({ trunc_width = 75 })
      --       local diagnostics = mini_status.section_diagnostics({ trunc_width = 75 })
      --       local lsp = statusline.section_lsp({ trunc_width = 75 })
      --       local filename = mini_status.section_filename({ trunc_width = 140 })
      --       local fileinfo = mini_status.section_fileinfo({ trunc_width = 120 })
      --       local location = mini_status.section_location({ trunc_width = 75 })
      --       local search = mini_status.section_searchcount({ trunc_width = 75 })
      --
      --       return mini_status.combine_groups({
      --         { hl = mode_hl, strings = { mode } },
      --         { hl = "MiniStatuslineDevinfo", strings = { git, diff, diagnostics, lsp } },
      --         "%<", -- Mark general truncate point
      --         { hl = "MiniStatuslineFilename", strings = { filename } },
      --         "%=", -- End left alignment
      --         { hl = "MiniStatuslineFileinfo", strings = { fileinfo } },
      --         { hl = mode_hl, strings = { search, location } },
      --       })
      --     end,
      --   },
      -- })

      local miniclue = require("mini.clue")
      miniclue.setup({
        triggers = {
          -- Leader triggers
          { mode = "n", keys = "<leader>" },
          { mode = "x", keys = "<leader>" },

          -- Built-in completion
          { mode = "i", keys = "<C-x>" },

          -- `g` key
          { mode = "n", keys = "g" },
          { mode = "x", keys = "g" },

          -- Marks
          { mode = "n", keys = "'" },
          { mode = "n", keys = "`" },
          { mode = "x", keys = "'" },
          { mode = "x", keys = "`" },

          -- Registers
          { mode = "n", keys = "\"" },
          { mode = "x", keys = "\"" },
          { mode = "i", keys = "<C-r>" },
          { mode = "c", keys = "<C-r>" },

          -- Window commands
          { mode = "n", keys = "<C-w>" },

          -- `z` key
          { mode = "n", keys = "z" },
          { mode = "x", keys = "z" },
        },
        clues = {
          { mode = "n", keys = "<leader>c", desc = "+Code" },
          { mode = "n", keys = "<leader>d", desc = "+Debugging" },
          { mode = "n ", keys = "<leader>f", desc = "+File" },
          { mode = "n ", keys = "<leader>g", desc = "+Git" },
          { mode = "n ", keys = "<leader>t", desc = "+Test" },
          { mode = "n ", keys = "<leader>s", desc = "+Settings" },
          { mode = "n ", keys = "<leader>b", desc = "+Buffer" },
          { mode = "n ", keys = "<leader>m", desc = "+Database" },
          miniclue.gen_clues.builtin_completion(),
          miniclue.gen_clues.g(),
          miniclue.gen_clues.marks(),
          miniclue.gen_clues.registers(),
          miniclue.gen_clues.windows(),
          miniclue.gen_clues.z(),
        },
      })
    end,
  },
}
