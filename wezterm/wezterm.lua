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
config.font_size = 12.0
config.line_height = 1.0
config.color_scheme = "Catppuccin Macchiato"

config.scrollback_lines = 5000
-- config.window_close_confirmation = "NeverPrompt"

-- Window appearance
-- config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"
-- config.integrated_title_button_style = "Gnome"
-- config.integrated_title_buttons = { "Hide", "Maximize", "Close" }

-- Screen DPI
config.dpi_by_screen = {
	["Build-in Retina Display"] = 144,
	["DELL S2722DZ"] = 81.75,
	["DELL P2421DC"] = 80,
}

-- Tab bar
config.hide_tab_bar_if_only_one_tab = true
config.show_tabs_in_tab_bar = true

config.window_padding = {
	left = 2,
	right = 2,
	top = 2,
	bottom = 0,
}

-- Keybindings
config.disable_default_key_bindings = true

config.keys = {
	{
		mods = "CMD|SHIFT",
		key = "v",
		action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }),
	},
	{
		mods = "CMD|SHIFT",
		key = "s",
		action = act.SplitVertical({ domain = "CurrentPaneDomain" }),
	},
	{
		mods = "CMD",
		key = "t",
		action = act.SpawnTab("CurrentPaneDomain"),
	},
	{
		mods = "CMD",
		key = "w",
		action = act.CloseCurrentPane({ confirm = true }),
	},
	{
		mods = "CMD",
		key = "+",
		action = act.IncreaseFontSize,
	},
	{
		mods = "CMD",
		key = "=",
		action = act.IncreaseFontSize,
	},
	{
		mods = "CMD",
		key = "-",
		action = act.DecreaseFontSize,
	},
	{
		mods = "CMD",
		key = "0",
		action = act.ResetFontSize,
	},
	{
		mods = "CMD",
		key = "k",
		action = act.Multiple({
			act.ClearScrollback("ScrollbackAndViewport"),
			act.SendKey({ key = "L", mods = "CTRL" }),
		}),
	},
	{
		mods = "CMD",
		key = "p",
		action = act.ActivateCommandPalette,
	},
	{
		mods = "CMD",
		key = "f",
		action = act.Multiple({
			act.CopyMode("ClearPattern"),
			act.Search({ CaseSensitiveString = "" }),
		}),
	},
	{
		mods = "CMD|SHIFT",
		key = "x",
		action = act.ActivateCopyMode,
	},
	{
		mods = "CMD|SHIFT",
		key = "h",
		action = act.ActivatePaneDirection("Left"),
	},
	{
		mods = "CMD|SHIFT",
		key = "j",
		action = act.ActivatePaneDirection("Down"),
	},
	{
		mods = "CMD|SHIFT",
		key = "k",
		action = act.ActivatePaneDirection("Up"),
	},
	{
		mods = "CMD|SHIFT",
		key = "l",
		action = act.ActivatePaneDirection("Right"),
	},
	{
		mods = "CMD|SHIFT",
		key = "LeftArrow",
		action = act.ActivatePaneDirection("Left"),
	},
	{
		mods = "CMD|SHIFT",
		key = "DownArrow",
		action = act.ActivatePaneDirection("Down"),
	},
	{
		mods = "CMD|SHIFT",
		key = "UpArrow",
		action = act.ActivatePaneDirection("Up"),
	},
	{
		mods = "CMD|SHIFT",
		key = "RightArrow",
		action = act.ActivatePaneDirection("Right"),
	},
	{
		mods = "CMD|SHIFT",
		key = "y",
		action = act.AdjustPaneSize({ "Left", 5 }),
	},
	{
		mods = "CMD|SHIFT",
		key = "u",
		action = act.AdjustPaneSize({ "Down", 5 }),
	},
	{
		mods = "CMD|SHIFT",
		key = "i",
		action = act.AdjustPaneSize({ "Up", 5 }),
	},
	{
		mods = "CMD|SHIFT",
		key = "o",
		action = act.AdjustPaneSize({ "Right", 5 }),
	},
	{
		mods = "CMD|SHIFT",
		key = "z",
		action = act.TogglePaneZoomState,
	},
	{
		mods = "CMD",
		key = "c",
		action = act.CopyTo("Clipboard"),
	},
	{
		mods = "CMD",
		key = "v",
		action = act.PasteFrom("Clipboard"),
	},
	{
		key = "e",
		mods = "CMD|SHIFT",
		action = act.PromptInputLine({
			description = wezterm.format({
				{ Attribute = { Intensity = "Bold" } },
				{ Foreground = { AnsiColor = "Fuchsia" } },
				{ Text = "Enter new name for tab" },
			}),
			action = wezterm.action_callback(function(window, _, line)
				if line then
					window:active_tab():set_title(line)
				end
			end),
		}),
	},
	-- Workspaces
	{
		key = "w",
		mods = "CMD|SHIFT",
		action = act.ShowLauncherArgs({ flags = "FUZZY|WORKSPACES" }),
	},
	{
		key = "n",
		mods = "CMD|SHIFT",
		action = act.PromptInputLine({
			description = wezterm.format({
				{ Attribute = { Intensity = "Bold" } },
				{ Foreground = { AnsiColor = "Fuchsia" } },
				{ Text = "Enter name for new workspace" },
			}),
			action = wezterm.action_callback(function(window, pane, line)
				if line then
					window:perform_action(
						act.SwitchToWorkspace({
							name = line,
						}),
						pane
					)
				end
			end),
		}),
	},
}

-- wezterm.on("update-right-status", function(window, pane)
-- 	window:set_right_status(window:active_workspace())
-- end)

for i = 1, 8 do
	-- SUPER + number to activate that tab
	table.insert(config.keys, {
		key = tostring(i),
		mods = "CMD",
		action = act.ActivateTab(i - 1),
	})

	-- F1 through F8 to activate that tab
	table.insert(config.keys, {
		key = "F" .. tostring(i),
		action = act.ActivateTab(i - 1),
	})
end

return config
