local wezterm = require("wezterm")
local act = wezterm.action

-- Add config folder to watchlist for config reloads.
wezterm.add_to_config_reload_watch_list(wezterm.config_dir)

local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- Font settings
config.font = wezterm.font("JetBrainsMono NF")
config.font_size = 10
config.line_height = 1.0
config.color_scheme = "Catppuccin Macchiato"

config.scrollback_lines = 5000
config.window_close_confirmation = "NeverPrompt"

-- Window appearance
-- config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
-- config.integrated_title_button_style = "Gnome"
-- config.integrated_title_buttons = { "Hide", "Maximize", "Close" }

-- Screen DPI
config.dpi_by_screen = {
    ['Build-in Retina Display'] = 144,
    ['DELL S2722DZ'] = 81.75,
    ['DELL P2421DC'] = 80,
}

-- Tab bar
config.hide_tab_bar_if_only_one_tab = true
config.show_tabs_in_tab_bar = true

config.window_padding = {
	left = 2,
	right = 0,
	top = 6,
	bottom = 0,
}

-- Keybindings
--config.disable_default_key_bindings = false

config.keys = {
	{
		key = "E",
		mods = "CTRL|SHIFT",
		action = act.PromptInputLine({
			description = "Enter new name for tab",
			action = wezterm.action_callback(function(window, _, line)
				if line then
					window:active_tab():set_title(line)
				end
			end),
		}),
	},
}

return config
