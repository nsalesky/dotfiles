return {
  {
    "williamboman/mason.nvim",
    opts = {},
  },
  {
    "williamboman/mason-lspconfig.nvim",
    opts = {
      ensure_installed = {
        "lua_ls",
        -- "rust_analyzer",
        -- "jedi_language_server",
      },
    },
  },
  -- {
  --     "jay-babu/mason-null-ls.nvim",
  --     opts = {
  --         ensure_installed = {
  --             -- Lua
  --             "stylua",
  --
  --             -- Python
  --             "ruff",
  --             "mypy",
  --             "black", -- code formatting
  --         },
  --         handlers = {},
  --     },
  -- },
  -- {
  --     "jay-babu/mason-nvim-dap.nvim",
  --     opts = {
  --         ensure_installed = {
  --             "debugpy",
  --         },
  --         handlers = {},
  --     },
  -- },
}
