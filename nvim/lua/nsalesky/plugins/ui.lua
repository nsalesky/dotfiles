return {
    {
        "folke/which-key.nvim",
        config = function()
            vim.o.timeout = true
            vim.o.timeoutlen = 300

            local wk = require("which-key")
            wk.setup({})
            wk.register({
                c = { name = "code" },
                d = { name = "debugging" },
                f = { name = "file" },
                g = { name = "git" },
                t = { name = "test" },
                s = { name = "settings" },
                b = { name = "buffer", s = { name = "sort" } },
                m = { name = "database" },
            }, { prefix = "<leader>" })
        end,
    },
    "nvim-tree/nvim-web-devicons",
    {
        "nvim-lualine/lualine.nvim",
        opts = require("nsalesky.configs.lualine"),
    },
    -- {
    --     "rcarriga/nvim-notify",
    --     config = function()
    --         vim.notify = require("notify")
    --     end,
    -- },
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
    --     "folke/noice.nvim",
    --     event = "VeryLazy",
    --     opts = {
    --         lsp = {
    --             -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
    --             override = {
    --                 ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
    --                 ["vim.lsp.util.stylize_markdown"] = true,
    --                 ["cmp.entry.get_documentation"] = true,
    --             },
    --         },
    --         -- you can enable a preset for easier configuration
    --         presets = {
    --             -- bottom_search = true, -- use a classic bottom cmdline for search
    --             command_palette = true, -- position the cmdline and popupmenu together
    --             long_message_to_split = true, -- long messages will be sent to a split
    --             inc_rename = false, -- enables an input dialog for inc-rename.nvim
    --             lsp_doc_border = true, -- add a border to hover docs and signature help
    --         },
    --     },
    --     dependencies = {
    --         "MunifTanjim/nui.nvim",
    --         "rcarriga/nvim-notify",
    --     },
    -- },
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
