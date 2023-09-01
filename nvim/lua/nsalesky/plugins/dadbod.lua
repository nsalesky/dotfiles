return {
    {
        "tpope/vim-dadbod",
        opt = true,
        dependencies = {
            "kristijanhusak/vim-dadbod-ui",
            "kristijanhusak/vim-dadbod-completion",
        },
        config = function()
            vim.g.db_ui_save_location = vim.fn.stdpath("data") .. require("plenary.path").path.sep .. "db_ui"

            vim.api.nvim_create_autocmd("FileType", {
                pattern = {
                    "sql",
                    "mysql",
                    "plsql",
                },
                callback = function()
                    require("cmp").setup.buffer({ sources = { { name = "vim-dadbod-completion" } } })
                end,
            })
        end,
        cmd = { "DBUIToggle", "DBUI", "DBUIAddConnection", "DBUIFindBuffer", "DBUIRenameBuffer", "DBUILastQueryInfo" },
        keys = {
            { "<leader>mu", ":DBUIToggle<CR>",        desc = "Toggle UI" },
            { "<leader>mf", ":DBUIFindBuffer<CR>",    desc = "Find buffer" },
            { "<leader>mr", ":DBUIRenameBuffer<CR>",  desc = "Rename buffer" },
            { "<leader>mq", ":DBUILastQueryInfo<CR>", desc = "Last query info" },
        },
    },
}
