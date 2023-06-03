
return {
    {
        "hrsh7th/nvim-cmp",
        config = function()
            require("nsalesky.configs.cmp")
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
