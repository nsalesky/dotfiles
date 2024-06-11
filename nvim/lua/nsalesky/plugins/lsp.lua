return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")
      local on_attach = require("nsalesky.configs.lspconfig").on_attach

      -- require("neodev").setup({})
      -- require("neoconf").setup({})

      lspconfig.lua_ls.setup({
        on_attach = on_attach,
        -- capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            runtime = {
              version = "LuaJIT",
            },
            workspace = {
              checkThirdParty = false,
            },
            telemetry = {
              enable = false,
            },
          },
        },
      })

      lspconfig.pyright.setup({
        on_attach = on_attach,
      })

      lspconfig.gopls.setup({
        on_attach = on_attach,
      })

      lspconfig.svelte.setup({
        on_attach = on_attach,
      })

      lspconfig.tsserver.setup({
        on_attach = on_attach,
      })

      lspconfig.ocamllsp.setup({
        on_attach = on_attach,
      })

      -- lspconfig.typst_lsp.setup({
      --   on_attach = on_attach,
      --   capabilities = capabilities,
      --   settings = {
      --     exportPdf = "onSave",
      --   },
      -- })

      lspconfig.hls.setup({
        on_attach = on_attach,
      })

      lspconfig.solargraph.setup({
        on_attach = on_attach,
        cmd = { "bundle", "exec", "solargraph", "stdio" },
      })

      lspconfig.tailwindcss.setup({
        on_attach = on_attach,
        filetypes = { "html", "css", "svelte", "eruby" },
        init_options = {
          userLanguages = {
            rust = "html",
          },
        },
      })

      lspconfig.clangd.setup({
        on_attach = on_attach,
      })
    end,
  },
}
