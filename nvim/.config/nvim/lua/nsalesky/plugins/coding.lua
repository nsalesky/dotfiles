return {
    {
        "numtostr/comment.nvim",
        config = function()
            require("Comment").setup({
                toggler = {
                    line = "<leader>/",
                },
                opleader = {
                    line = "<leader>/",
                },
            })
        end,
    },
    {
       'mfussenegger/nvim-dap',
        config = function()
            vim.keymap.set('n', '<leader>db', '<cmd> DapToggleBreakpoint <CR>', { desc = "Toggle Breakpoint" })
            vim.keymap.set('n', '<leader>ds', function()
                local widgets = require("dap.ui.widgets")
                local sidebar = widgets.sidebar(widgets.scopes)
                sidebar.open()
            end, { desc = "Open Sidebar" })
        end,
    },
}
