
return {
    {
        "hrsh7th/nvim-cmp",
        opts = function()
            return require("nsalesky.configs.cmp")
        end,
        config = function(_, opts)
            require("cmp").setup(opts)
        end,
    },
    "hrsh7th/cmp-nvim-lsp",
    {
        "L3MON4D3/LuaSnip",
        version = "1",
        build = "make install_jsregexp", -- optional
    },
    "onsails/lspkind.nvim",
}
