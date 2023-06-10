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
        config = function()
            vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
        end,
    },
    {
        "christoomey/vim-tmux-navigator",
        lazy = false,
    },
    {
        "stevearc/oil.nvim",
        config = function()
            local oil = require("oil")

            oil.setup()

            vim.keymap.set("n", "-", oil.open, { desc = "Open parent directory" })
        end,
    },
}
