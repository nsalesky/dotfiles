return {
  {
    "neovim/nvim-lspconfig",
    dependencies = { "saghen/blink.cmp" },
    opts = {
      servers = {
        lua_ls = {
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
        },
        basedpyright = {
          settings = {
            basedpyright = {
              disableOrganizeImports = true,
              analysis = {
                typeCheckingMode = "standard", -- off, basic, standard, strict, all
                autoImportCompletions = true,
                useLibraryCodeForTypes = true,
              },
            },
          },
        },
        rust_analyzer = {},
        bashls = {},
        gopls = {},
        svelte = {},
        hls = {},
        clangd = {
          cmd = {
            "clangd",
            "--background-index",
            "--clang-tidy",
            "--completion-style=bundled",
            "--cross-file-rename",
            "--header-insertion=iwyu",
          },
        },
      },
      yamlls = {
        settings = {
          yaml = {
            schemes = {
              ["https://raw.githubusercontent.com/docker/cli/master/cli/compose/schema/data/config_schema_v3.13.json"] = "./docker-compose.yml",
            },
          },
        },
      },
    },
    config = function(_, opts)
      local lspconfig = require("lspconfig")
      local blink = require("blink.cmp")
      local on_attach = require("nsalesky.configs.lspconfig").on_attach

      for server, config in pairs(opts.servers) do
        config.capabilities = blink.get_lsp_capabilities(config.capabilities)
        config.on_attach = on_attach
        lspconfig[server].setup(config)
      end

      lspconfig.ruff.setup({
        on_attach = function(client, _)
          client.server_capabilities.hoverProvider = false
        end,
      })
    end,
  },
}
