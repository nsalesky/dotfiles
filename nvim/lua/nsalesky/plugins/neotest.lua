return {
    "nvim-neotest/neotest",
    dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-treesitter/nvim-treesitter",
        "antoinemadec/FixCursorHold.nvim",

        -- Adapters
        "nvim-neotest/neotest-python",
        "nvim-neotest/neotest-plenary",
    },
    config = function()
        require("neotest").setup({
            adapters = {
                require("neotest-python")({
                    dap = { justMyCode = false },
                }),
                require("neotest-plenary"),
            },
        })
    end,
    keys = {
        {
            "<leader>tt",
            function()
                require("neotest").run.run()
            end,
            desc = "Run nearest test",
        },
        {
            "<leader>tf",
            function()
                require("neotest").run.run(vim.fn.expand("%"))
            end,
            desc = "Run current file",
        },
        {
            "<leader>td",
            function()
                require("neotest").run.run({ strategy = "dap" })
            end,
            desc = "Debug nearest test",
        },
        {
            "<leader>ts",
            function()
                require("neotest").run.stop()
            end,
            desc = "Stop nearest test",
        },
        {
            "<leader>ta",
            function()
                require("neotest").run.attach()
            end,
            desc = "Attach to nearest test",
        },
    },
}
