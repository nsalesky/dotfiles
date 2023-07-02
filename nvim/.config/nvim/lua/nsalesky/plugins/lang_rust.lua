return {
    {
        "rust-lang/rust.vim",
        init = function()
            vim.g.rustfmt_autosave = 1
        end,
    },
    {
        "simrat39/rust-tools.nvim",
        ft = "rust",
        dependencies = "neovim/nvim-lspconfig",
        config = function()
            local on_attach = require("nsalesky.configs.lspconfig").on_attach
            local capabilities = require("nsalesky.configs.lspconfig").capabilities

            require("rust-tools").setup({
                server = {
                    on_attach = on_attach,
                    capabilities = capabilities,
                },
            })
        end,
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
