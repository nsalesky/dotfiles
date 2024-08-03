return {
  {
    "folke/trouble.nvim",
    opts = {},
    cmd = "Trouble",
    keys = {
      {
        "<leader>xx",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      },
      {
        "<leader>xX",
        "<cmd>Trouble diagnostics toggle filter.buf=0<cr>",
        desc = "Buffer Diagnostics (Trouble)",
      },
    },
  },
  -- {
  --   "folke/neodev.nvim",
  -- },
  -- {
  --   "folke/neoconf.nvim",
  -- },
  -- {
  --   "kylechui/nvim-surround",
  --   version = "*",
  --   event = "VeryLazy",
  --   opts = {},
  -- },
  -- {
  --   "folke/todo-comments.nvim",
  --   dependencies = "nvim-lua/plenary.nvim",
  --   event = "VeryLazy",
  --   opts = {},
  -- },
  -- {
  --   "windwp/nvim-autopairs",
  --   event = "InsertEnter",
  --   config = true,
  --   opts = {},
  -- },
  -- {
  --   "luckasRanarison/tailwind-tools.nvim",
  --   opts = {}
  -- },
  -- {
  --   "olical/conjure",
  --   ft = { "clojure", "lua", "fennel", "ocaml" },
  --   dependencies = {
  --     "PaterJason/cmp-conjure",
  --     -- config = function()
  --     --   local cmp = require("cmp")
  --     --   local config = cmp.get_config()
  --     --   table.insert(config.sources, {
  --     --     name = "buffer",
  --     --     option = {
  --     --       sources = {
  --     --         { name = "conjure" },
  --     --       },
  --     --     },
  --     --   })
  --     --   cmp.setup(config)
  --     -- end,
  --   },
  --   config = function(_, opts)
  --     require("conjure.main").main()
  --     require("conjure.mapping")["on-filetype"]()
  --   end,
  --   init = function()
  --     vim.g["conjure#filetypes"] = {"clojure", "fennel", "janet", "hy", "julia", "racket", "scheme", "lua", "lisp", "python", "rust", "sql", "ocaml"}
  --     vim.g["conjure#ocaml"] = "nsalesky.configs.conjure_ocaml"
  --   end
  -- }
}
