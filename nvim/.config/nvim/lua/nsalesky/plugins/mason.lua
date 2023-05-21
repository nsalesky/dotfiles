return {
    {
        "williamboman/mason.nvim",
        config = function()
            require("mason").setup()
        end,
    },
    {
        "williamboman/mason-lspconfig.nvim",
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
				    "lua_ls",
                    -- "sumneko_lua",
				    "rust_analyzer",
                    "jedi_language_server",
                },
            })
        end,
    },
}
