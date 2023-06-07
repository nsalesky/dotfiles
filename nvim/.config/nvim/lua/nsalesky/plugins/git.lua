return {
    {
        "kdheepak/lazygit.nvim",
        keys = {
            {
                "<leader>gs",
                ":LazyGit<CR>",
                desc = "Test method",
            },
        },
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
}
