return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require("lspconfig")
      local on_attach = require("nsalesky.configs.lspconfig").on_attach
      local capabilities = require("nsalesky.configs.lspconfig").capabilities

      -- require("neodev").setup({})
      -- require("neoconf").setup({})

      lspconfig.lua_ls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim" },
            },
            runtime = {
              version = "LuaJIT",
            },
            workspace = {
              library = { vim.env.VIMRUNTIME },
              checkThirdParty = false,
            },
            telemetry = {
              enable = false,
            },
          },
        },
      })

      lspconfig.rust_analyzer.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.pyright.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.gopls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.svelte.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.tsserver.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.ocamllsp.setup({
        on_attach = on_attach,
        capabilities = capabilities,
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
        capabilities = capabilities,
      })

      lspconfig.solargraph.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        cmd = { "bundle", "exec", "solargraph", "stdio" },
      })

      lspconfig.tailwindcss.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        filetypes = { "html", "css", "svelte", "eruby" },
        init_options = {
          userLanguages = {
            rust = "html",
          },
        },
      })

      lspconfig.clangd.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        cmd = {
          "clangd",
          "--background-index",
          "--clang-tidy",
          "--completion-style=bundled",
          "--cross-file-rename",
          "--header-insertion=iwyu",
        },
      })
    end,
  },
}
