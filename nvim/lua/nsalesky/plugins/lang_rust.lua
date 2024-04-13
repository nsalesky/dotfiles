return {
  -- {
  --   "rust-lang/rust.vim",
  --   init = function()
  --     vim.g.rustfmt_autosave = 1
  --   end,
  -- },
  {
    "mrcjkb/rustaceanvim",
    version = "^4",
    ft = { "rust" },
    init = function()
      local on_attach = require("nsalesky.configs.lspconfig").on_attach

      -- TODO: neotest integration is not working for me
      vim.g.rustaceanvim = {
        server = {
          on_attach = on_attach,
        }
      }

    end
  },
  {
    "saecki/crates.nvim",
    ft = { "rust", "toml" },
    config = function()
      local crates = require("crates")
      crates.setup({})
      crates.show()
    end,
  },
}
