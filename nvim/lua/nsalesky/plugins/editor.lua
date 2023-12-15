return {
    {
        "theprimeagen/harpoon",
        opts = {},
        keys = {
            {
                "<leader>a", function() require("harpoon.mark").add_file() end, desc = "Add current file to Harpoon",
            },
            {
                "<C-e>", function() require("harpoon.ui").toggle_quick_menu() end, desc = "Open Harpoon quick menu",
            },
            {
                "<leader>h", function() require("harpoon.ui").nav_file(1) end, desc = "Goto Harpoon 1",
            },
            {
                "<leader>j", function() require("harpoon.ui").nav_file(2) end, desc = "Goto Harpoon 2",
            },
            {
                "<leader>k", function() require("harpoon.ui").nav_file(3) end, desc = "Goto Harpoon 3",
            },
            {
                "<leader>l", function() require("harpoon.ui").nav_file(4) end, desc = "Goto Harpoon 4",
            },
        },
    },
    {
        "mbbill/undotree",
        opts = {},
        keys = {
            {
                "<leader>u",
                ":UndotreeToggle<CR>",
                desc = "Undotree Toggle",
            },
        },
    },
    {
        "aserowy/tmux.nvim",
        lazy = false,
        opts = {
            copy_sync = {
                enable = false,
            },
            navigation = {
                cycle_navigation = true,
                enable_default_keybindings = true,
                persist_zoom = true,
            },
            resize = {
                enable_default_keybindings = true,
                resize_step_x = 1,
                resize_step_y = 1,
            },
        },
        config = function(_, opts)
            require("tmux").setup(opts)
        end,
    },
    {
        "tpope/vim-obsession",
    },
}
