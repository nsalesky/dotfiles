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
			}, { prefix = "<leader>" })
		end,
	},
    'nvim-tree/nvim-web-devicons',
    {
        'nvim-lualine/lualine.nvim',
        opts = require("nsalesky.configs.lualine"),
        config = function(_, opts)
            require("lualine").setup(opts)
        end,
    },
    {
        "rcarriga/nvim-notify",
        config = function()
            vim.notify = require("notify")
        end,
    },
    -- {
    --     "romgrk/barbar.nvim",
    --     dependencies = {
    --         "lewis6991/gitsigns.nvim", -- OPTIONAL: for git status
    --         "nvim-tree/nvim-web-devicons", -- OPTIONAL: for file icons
    --     },
    --     init = function() vim.g.barbar_auto_setup = false end,
    --     opts = {},
    --     version = '^1.0.0', -- optional: only update when a new 1.x version is released
    -- },
}
