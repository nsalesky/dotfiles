return {
	'nvim-telescope/telescope.nvim', tag = '0.1.1',
	-- or                              , branch = '0.1.1',
	dependencies = { 'nvim-lua/plenary.nvim' },
	config = function()
		local builtin = require('telescope.builtin')

		vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = "Find File" })
		vim.keymap.set('n', '<C-p>', builtin.git_files, { desc = "Find Git File" })
		vim.keymap.set('n', '<leader>fs', builtin.grep_string, { desc = "Search For Text" })
	end,
}
