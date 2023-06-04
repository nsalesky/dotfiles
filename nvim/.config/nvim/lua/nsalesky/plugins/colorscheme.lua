return {
    "catppuccin/nvim",
    config = function()
        require("catppuccin").setup({
            flavour = "mocha",
            integrations = {
                cmp = true,
                gitsigns = true,
                telescope = true,
                notify = true,
                lsp_trouble = true,
                nvimtree = true,
            },
        })

        vim.cmd.colorscheme("catppuccin")
    end,
}
