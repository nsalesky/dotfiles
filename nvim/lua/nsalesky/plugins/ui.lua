return {
    {
        "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300

            local wk = require("which-key")
            wk.setup({})
            wk.register({
                c = { "code" },
                d = { "debugging" },
                f = { "file" },
                g = { "git" },
                u = { "Undo Tree" },
                m = { "Database" },
            }, { prefix = "<leader>" })
        end,
    },
    "nvim-tree/nvim-web-devicons",
    {
        "nvim-lualine/lualine.nvim",
        opts = require("nsalesky.configs.lualine"),
    },
    {
        "rcarriga/nvim-notify",
        config = function()
            vim.notify = require("notify")
        end,
    },
    {
        "nvim-tree/nvim-tree.lua",
        opts = function()
            return require("nsalesky.configs.nvim-tree")
        end,
        cmd = { "NvimTreeToggle", "NvimTreeFocus" },
        keys = {
            {
                "<C-n>",
                "<cmd>NvimTreeToggle<CR>",
                desc = "Toggle nvimtree",
            },
            {
                "<leader>e",
                "<cmd>NvimTreeFocus<CR>",
                desc = "Focus nvimtree",
            },
        },
    },
    -- {
    --     "romgrk/barbar.nvim",
    --     dependencies = {
    --         "lewis6991/gitsigns.nvim",     -- OPTIONAL: for git status
    --         "nvim-tree/nvim-web-devicons", -- OPTIONAL: for file icons
    --     },
    --     init = function()
    --         vim.g.barbar_auto_setup = false
    --     end,
    --     opts = {
    --         auto_hide = true,
    --         sidebar_filetypes = {
    --             NvimTree = true,
    --             undotree = true,
    --         },
    --     },
    --     version = "^1.0.0", -- optional: only update when a new 1.x version is released
    -- },
}
