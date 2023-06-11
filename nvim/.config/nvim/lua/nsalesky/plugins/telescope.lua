return {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
        local builtin = require("telescope.builtin")

        vim.keymap.set("n", "<leader>ff", function()
            builtin.find_files({
                hidden = true,
            })
        end, { desc = "Find File" })

        vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Find Buffer" })
        vim.keymap.set("n", "<C-p>", builtin.git_files, { desc = "Find Git File" })
        vim.keymap.set("n", "<leader>fs", builtin.live_grep, { desc = "Search For Text" })
        vim.keymap.set("n", "<leader>tt", builtin.colorscheme, { desc = "Color scheme" })

        -- Set up some custom theming
        require("nsalesky.color_scheme").set_telescope_colors()

        -- Open Telescope results in trouble
        local telescope = require("telescope")
        -- local actions = require("telescope.actions")
        local trouble = require("trouble.providers.telescope")

        telescope.setup({
            defaults = {
                mappings = {
                    i = {
                        ["<c-t>"] = trouble.open_with_trouble,
                    },
                    n = {
                        ["<c-t>"] = trouble.open_with_trouble,
                    },
                },
            },
        })
    end,
}
