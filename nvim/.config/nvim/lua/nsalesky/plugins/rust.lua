return {
    {
        "simrat39/rust-tools.nvim",
        ft = "rust",
        dependencies = "neovim/nvim-lspconfig",
        opts = require("nsalesky.configs.lspconfig"),
        config = function(_, opts)
            require("rust-tools").setup(opts)
        end,
    },
}
