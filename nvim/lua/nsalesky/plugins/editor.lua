return {
    {
        "theprimeagen/harpoon",
        config = function()
            local mark = require("harpoon.mark")
            local ui = require("harpoon.ui")

            vim.keymap.set("n", "<leader>a", mark.add_file, { desc = "Add current file to Harpoon" })
            vim.keymap.set("n", "<C-e>", ui.toggle_quick_menu, { desc = "Open Harpoon quick menu" })
            vim.keymap.set("n", "<leader>h", function()
                ui.nav_file(1)
            end, { desc = "Goto Harpoon 1" })
            vim.keymap.set("n", "<leader>j", function()
                ui.nav_file(2)
            end, { desc = "Goto Harpoon 2" })
            vim.keymap.set("n", "<leader>k", function()
                ui.nav_file(3)
            end, { desc = "Goto Harpoon 3" })
            vim.keymap.set("n", "<leader>l", function()
                ui.nav_file(4)
            end, { desc = "Goto Harpoon 4" })
        end,
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
    -- {
    --     "stevearc/oil.nvim",
    --     opts = {},
    --     -- keys = {
    --     --     {
    --     --         "-",
    --     --         function()
    --     --             require("oil").open()
    --     --         end,
    --     --         desc = "Open parent directory",
    --     --     },
    --     -- },
    -- },
}
