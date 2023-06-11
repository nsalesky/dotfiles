local M = {}

function M.get_lualine_colors()
	local c = require("catppuccin.palettes").get_palette()

	return {
		outer_pill_text = c.black,
		inner_pill_text = c.text,
		inner_pill_bg = c.surface1,
		inner_pill_icon = c.mauve,
		inner_bg = c.black,
	}
end

-- Set up the overall Neovim colorscheme
function M.set_initial_colorscheme()
	vim.cmd.colorscheme("catppuccin-macchiato")
end

-- Set up some custom Telescope theming
function M.set_telescope_colors()
	local colors = require("catppuccin.palettes").get_palette()
	local telescope_colors = {
		TelescopeMatching = { fg = colors.flamingo },
		TelescopeSelection = { fg = colors.text, bg = colors.surface0, bold = true },

		TelescopePromptPrefix = { bg = colors.surface0 },
		TelescopePromptNormal = { bg = colors.surface0 },
		TelescopeResultsNormal = { bg = colors.mantle },
		TelescopePreviewNormal = { bg = colors.mantle },
		TelescopePromptBorder = { bg = colors.surface0, fg = colors.surface0 },
		TelescopeResultsBorder = { bg = colors.mantle, fg = colors.mantle },
		TelescopePreviewBorder = { bg = colors.mantle, fg = colors.mantle },
		TelescopePromptTitle = { bg = colors.pink, fg = colors.mantle },
		TelescopeResultsTitle = { fg = colors.mantle },
		TelescopePreviewTitle = { bg = colors.green, fg = colors.mantle },
	}

	for hl, col in pairs(telescope_colors) do
		vim.api.nvim_set_hl(0, hl, col)
	end
end

return M
