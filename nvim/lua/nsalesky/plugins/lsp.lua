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

      -- lspconfig.pyright.setup({
      --   on_attach = on_attach,
      --   capabilities = capabilities,
      -- })

      lspconfig.basedpyright.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          basedpyright = {
            disableOrganizeImports = true,
            analysis = {
              typeCheckingMode = "standard", -- off, basic, standard, strict, all
              -- diagnosticsMode = "openFilesOnly", -- workspace, openFilesOnly
              autoImportCompletions = true,
              useLibraryCodeForTypes = true,
              diagnosticSeverityOverrides = {
                reportMissingTypeStubs = false,
              },
            },
          },
        },
      })

      lspconfig.ruff.setup({
        on_attach = function(client, _)
          client.server_capabilities.hoverProvider = false
        end,
      })

      lspconfig.gopls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
      })

      lspconfig.svelte.setup({
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

      lspconfig.yamlls.setup({
        on_attach = on_attach,
        capabilities = capabilities,
        settings = {
          yaml = {
            schemes = {
              ["https://raw.githubusercontent.com/docker/cli/master/cli/compose/schema/data/config_schema_v3.13.json"] = "./docker-compose.yml",
            },
          },
        },
      })
    end,
  },
}
