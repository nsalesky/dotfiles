return {
    {
        "lewis6991/gitsigns.nvim",
        opts = {
            current_line_blame = false,
            current_line_blame_opts = {
                virt_text = true,
                virt_text_pos = "eol",
                delay = 1000,
                ignore_whitespace = false,
            },
            current_line_blame_formatter = "<author>, <author_time:%Y-%m-%d> - <summary>",
        },
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
        dependencies = { "nvim-lua/plenary.nvim" },
        opts = {
            integrations = {
                diffview = true,
                telescope = true,
            },
            use_telescope = true,
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
        "ThePrimeagen/git-worktree.nvim",
        config = function()
            require("telescope").load_extension("git_worktree")
        end,
        keys = {
            {
                "<leader>gw",
                function()
                    require("telescope").extensions.git_worktree.git_worktrees()
                end,
                desc = "Git worktrees",
            },
            {
                "<leader>gc",
                function()
                    require("telescope").extensions.git_worktree.create_git_worktree()
                end,
                desc = "Create a worktree",
            },
        },
    },
    {
        "sindrets/diffview.nvim",
    },
    -- {
    --     "kdheepak/lazygit.nvim",
    --     keys = {
    --         {
    --             "<leader>gs",
    --             ":LazyGit<CR>",
    --         },
    --     },
    -- },
}
