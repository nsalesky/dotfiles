return {
    {
       "mfussenegger/nvim-dap",
        config = function()
            vim.keymap.set('n', '<leader>db', ':DapToggleBreakpoint <CR>', { desc = "Toggle Breakpoint" })
            vim.keymap.set('n', '<leader>dx', ':DapTerminate <CR>', { desc = "Terminate Debugger" })
            vim.keymap.set('n', '<leader>do', ':DapStepOver <CR>', { desc = "Step Over" })
            vim.keymap.set('n', '<leader>di', ':DapStepInto <CR>', { desc = "Step Into" })
            vim.keymap.set('n', '<leader>dc', ':DapContinue <CR>', { desc = "Continue Execution" })
        end,
    },
    {
        "rcarriga/nvim-dap-ui",
        dependencies = { "mfussenegger/nvim-dap" },
        config = function()
            local dapui = require("dapui")
            local dap = require("dap")

            dapui.setup({})

            dap.listeners.after.event_initialized["dapui_config"] = function()
                dapui.open()
            end

            dap.listeners.before.event_terminated["dapui_config"] = function()
                dapui.close()
            end

            dap.listeners.before.event_exited["dapui_config"] = function()
                dapui.close()
            end

        end
    },

}