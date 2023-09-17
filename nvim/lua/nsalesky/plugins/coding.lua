return {
    {
        "folke/neodev.nvim",
    },
    {
        "folke/neoconf.nvim",
    },
    {
        "numtostr/comment.nvim",
        opts = {
            toggler = {
                line = "<leader>/",
            },
            opleader = {
                line = "<leader>/",
            },
        },
    },
    {
        "mhartington/formatter.nvim",
        opts = function()
            return {
                logging = false,
                filetype = {
                    lua = {
                        require("formatter.filetypes.lua").stylua,
                    },
                    python = {
                        require("formatter.filetypes.python").black,
                    },
                    go = {
                        require("formatter.filetypes.go").gofmt,
                    },
                    ["*"] = {
                        require("formatter.filetypes.any").remove_trailing_whitespace,
                    },
                },
            }
        end,
        config = function(_, opts)
            require("formatter").setup(opts)
            vim.cmd("autocmd BufWritePost * FormatWrite") -- auto format on save
        end,
    },
    {
        "kylechui/nvim-surround",
        version = "*",
        event = "VeryLazy",
        opts = {},
    },
    {
        "folke/todo-comments.nvim",
        dependencies = "nvim-lua/plenary.nvim",
        event = "VeryLazy",
        opts = {},
    },
    {
        "windwp/nvim-autopairs",
        opts = {
            map_cr = false,
        },
    },
    {
        "folke/trouble.nvim",
        opts = {},
        keys = {
            { "<leader>xx", "<cmd>TroubleToggle<cr>", desc = "Trouble Toggle" },
            { "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Trouble Workspace" },
            { "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Trouble Document" },
            { "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", desc = "Trouble Quickfix" },
            { "<leader>gR", "<cmd>TroubleToggle lsp_references<cr>", desc = "LSP References" },
        },
    },
    -- {
    --     "michaelb/sniprun",
    --     event = "VeryLazy",
    --     build = "sh ./install.sh",
    --     opts = {}
    -- }
}
