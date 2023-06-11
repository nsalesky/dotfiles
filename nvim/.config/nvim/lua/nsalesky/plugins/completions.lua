return {
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "saadparwaiz1/cmp_luasnip",
        },
        opts = function()
            return require("nsalesky.configs.cmp")
        end,
    },
    "hrsh7th/cmp-nvim-lsp",
    {
        "L3MON4D3/LuaSnip",
        version = "1",
        build = "make install_jsregexp", -- optional
        dependencies = {
            "rafamadriz/friendly-snippets", -- adds a bunch of preconfigured snippets
        },
        config = function()
            require("luasnip.loaders.from_vscode").lazy_load()
        end,
    },
}
