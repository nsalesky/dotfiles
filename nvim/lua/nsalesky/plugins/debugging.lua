return {
    {
        "mfussenegger/nvim-dap",
        keys = {
            { "<leader>db", ":DapToggleBreakpoint<CR>", desc = "Toggle breakpoint" },
            { "<leader>do", ":DapStepOver <CR>", desc = "Step over" },
            { "<leader>di", ":DapStepInto <CR>", desc = "Step Into" },
            { "<leader>dc", ":DapContinue <CR>", desc = "Continue Execution" },
        },
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
        end,
    },
}
