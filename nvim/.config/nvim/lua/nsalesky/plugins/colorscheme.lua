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
            },
        })

        vim.cmd.colorscheme "catppuccin"
    end,
}
