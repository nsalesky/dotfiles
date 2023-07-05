return {
    {
        "kdheepak/lazygit.nvim",
        -- keys = {
        --     {
        --         "<leader>gs",
        --         ":LazyGit<CR>",
        --     },
        -- },
    },
    {
        "lewis6991/gitsigns.nvim",
        opts = {},
        keys = {
            {
                "<leader>gb",
                ":Gitsigns toggle_current_line_blame<CR>",
                desc = "Toggle line blame",
            },
        },
    },
    {
        "NeogitOrg/neogit",
        opts = {
            integrations = {
                diffview = true,
            },
        },
        keys = {
            {
                "<leader>gs",
                function()
                    require("neogit").open()
                end,
                desc = "Neogit status",
            },
        },
    },
    {
        "sindrets/diffview.nvim",
    },
}
