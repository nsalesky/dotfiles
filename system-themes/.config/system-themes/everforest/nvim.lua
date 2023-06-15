local M = {}

-- local configuration = vim.fn["everforest#get_configuration"]()
-- local palette = vim.fn["everforest#get_palette"](configuration.background, configuration.colors_override)

-- this is a bit of a hack since I couldn't get the above lines working
-- this works, but it won't adjust if I switch to a different variant
local colors = {
	fg = "#D3C6AA",
	bg0 = "#2D353B",
	bg1 = "#343F44",
	bg2 = "#3D484D",
	bg3 = "#475258",
	bg4 = "#4F585E",
	grey1 = "#grey1",
}

function M.get_lualine_colors()
	return {
		outer_pill_text = colors.bg0,
		inner_pill_text = colors.fg,
		inner_pill_icon = colors.fg,
		inner_pill_bg = colors.bg3,
		inner_bg = colors.bg1,
		inner_fg = colors.grey1,
	}
end

-- Set up the overall Neovim colorscheme
function M.set_initial_colorscheme()
	vim.cmd.colorscheme("everforest")
end

-- Set up some custom Telescope theming
function M.set_telescope_colors()
	-- local colors = require("catppuccin.palettes").get_palette()
	-- local telescope_colors = {
	--     TelescopeMatching = { fg = colors.flamingo },
	--     TelescopeSelection = { fg = colors.text, bg = colors.surface0, bold = true },
	--
	--     TelescopePromptPrefix = { bg = colors.surface0 },
	--     TelescopePromptNormal = { bg = colors.surface0 },
	--     TelescopeResultsNormal = { bg = colors.mantle },
	--     TelescopePreviewNormal = { bg = colors.mantle },
	--     TelescopePromptBorder = { bg = colors.surface0, fg = colors.surface0 },
	--     TelescopeResultsBorder = { bg = colors.mantle, fg = colors.mantle },
	--     TelescopePreviewBorder = { bg = colors.mantle, fg = colors.mantle },
	--     TelescopePromptTitle = { bg = colors.pink, fg = colors.mantle },
	--     TelescopeResultsTitle = { fg = colors.mantle },
	--     TelescopePreviewTitle = { bg = colors.green, fg = colors.mantle },
	-- }
	--
	-- for hl, col in pairs(telescope_colors) do
	--     vim.api.nvim_set_hl(0, hl, col)
	-- end
end

return M
