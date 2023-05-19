return {
	{
		'nvim-treesitter/nvim-treesitter', 
		build = ':TSUpdate',
		opts = {
			auto_install = true,
			highlight = { 
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = { enable = true },
			ensure_installed = {
				"bash",
				"json",
				-- "help",
				"lua",
				"luadoc",
				"luap",
				"python",
				"regex",
				"rust",
				"vim",
				"vimdoc",
				"yaml",
			},
		},
		config = function(_, opts)
			require("nvim-treesitter.configs").setup(opts)
		end,
	},
	{
		"nvim-treesitter/playground"
	},
}
