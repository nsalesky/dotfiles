return {
    "neovim/nvim-lspconfig",
    config = function()
        local lspconfig = require("lspconfig")
        local on_attach = require("nsalesky.configs.lspconfig").on_attach
        local capabilities = require("nsalesky.configs.lspconfig").capabilities

        lspconfig.lua_ls.setup({
            on_attach = on_attach,
            capabilities = capabilities,
        })
    end,
}

