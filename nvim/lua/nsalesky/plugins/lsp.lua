return {
    {
        "neovim/nvim-lspconfig",
        config = function()
            local lspconfig = require("lspconfig")
            local on_attach = require("nsalesky.configs.lspconfig").on_attach

            local capabilities = require("nsalesky.configs.lspconfig").capabilities

            require("neodev").setup({})
            require("neoconf").setup({})

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
                            checkThirdParty = false,
                        },
                        telemetry = {
                            enable = false,
                        },
                    },
                },
            })

            -- lspconfig.jedi_language_server.setup({
            --     on_attach = on_attach,
            --     capabilities = capabilities,
            -- })

            lspconfig.pylsp.setup({
                on_attach = on_attach,
                capabilities = capabilities,
            })
        end,
    },
    {
        "jose-elias-alvarez/null-ls.nvim",
        opts = function()
            return require("nsalesky.configs.null-ls")
        end,
    },
    {
        "glepnir/lspsaga.nvim",
        event = "LspAttach",
        config = function()
            require("lspsaga").setup({})
        end,
    },
}
