return {
    {
        "jose-elias-alvarez/null-ls.nvim",
        opts = function()
            return require("nsalesky.configs.null-ls")
        end,
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
        }
    },
    {
        "kylechui/nvim-surround",
        version = "*",
        event = "VeryLazy",
        opts = {},
    },
    {
        "folke/todo-comments.nvim",
        requires = "nvim-lua/plenary.nvim",
    },
    {
        "windwp/nvim-autopairs",
        opts = {
            map_cr = true
        }
    },
    {
        "folke/trouble.nvim",
        opts = {},
        keys = {
            { "<leader>xx", "<cmd>TroubleToggle<cr>", desc = "Trouble Toggle" },
            { "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", desc = "Trouble Workspace" },
            { "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", desc = "Trouble Document" },
            { "<leader>xq", "<cmd>TroubleToggle quickfix<cr>", desc = "Trouble Quickfix" },
            { "<leader>gR", "<cmd>TroubleToggle lsp_references<cr>", desc = "LSP References" }
        }
    }
}
