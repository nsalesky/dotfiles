return {
  {
    "folke/neodev.nvim",
  },
  {
    "folke/neoconf.nvim",
  },
  {
    "numtostr/comment.nvim",
    opts = {
      toggler = {
        line = "<leader>/",
      },
      opleader = {
        line = "<leader>/",
      },
    },
  },
  {
    "mhartington/formatter.nvim",
    opts = function()
      local util = require("formatter.util")

      return {
        logging = true,
        log_level = vim.log.levels.WARN,
        filetype = {
          lua = {
            require("formatter.filetypes.lua").stylua,
          },
          ocaml = {
            require("formatter.filetypes.ocaml").ocamlformat,
          },
          python = {
            require("formatter.filetypes.python").black,
          },
          ruby = {
            function()
              return {
                exe = "rubocop",
                args = {
                  "--fix-layout",
                  "--autocorrect-all",
                  "--stdin",
                  util.escape_path(util.get_current_buffer_file_name()),
                  "--format",
                  "files",
                  "--stderr",
                },
                stdin = true,
              }
            end,
          },
          go = {
            require("formatter.filetypes.go").gofmt,
          },
          ["*"] = {
            require("formatter.filetypes.any").remove_trailing_whitespace,
          },
        },
      }
    end,
    config = function(_, opts)
      require("formatter").setup(opts)
      -- vim.cmd("autocmd BufWritePost * FormatWrite") -- auto format on save
    end,
  },
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    opts = {},
  },
  {
    "folke/todo-comments.nvim",
    dependencies = "nvim-lua/plenary.nvim",
    event = "VeryLazy",
    opts = {},
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = true,
    opts = {},
  },
  -- {
  --   "luckasRanarison/tailwind-tools.nvim",
  --   opts = {}
  -- },
  {
    "olical/nfnl",
    ft = "fennel",
  },
  {
    "olical/conjure",
    ft = { "clojure", "lua", "fennel", "ocaml" },
    dependencies = {
      "PaterJason/cmp-conjure",
      { "nsalesky/conjure-ocaml", dev = true },
      -- config = function()
      --   local cmp = require("cmp")
      --   local config = cmp.get_config()
      --   table.insert(config.sources, {
      --     name = "buffer",
      --     option = {
      --       sources = {
      --         { name = "conjure" },
      --       },
      --     },
      --   })
      --   cmp.setup(config)
      -- end,
    },
    config = function(_, opts)
      require("conjure.main").main()
      require("conjure.mapping")["on-filetype"]()
    end,
    init = function()
      vim.g["conjure#filetypes"] = {"clojure", "fennel", "janet", "hy", "julia", "racket", "scheme", "lua", "lisp", "python", "rust", "sql", "ocaml"}
      vim.g["conjure#ocaml"] = "nsalesky.configs.conjure_ocaml"
    end
  }
}
