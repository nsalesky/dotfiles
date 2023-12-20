local M = {}

M.border_chars_none = { "", "", "", "", "", "", "", "" }
M.border_chars_empty = { " ", " ", " ", " ", " ", " ", " ", " " }
M.border_chars_tmux = { " ", " ", " ", " ", " ", " ", " ", " " }
M.border_chars_inner_thick = { " ", "▄", " ", "▌", " ", "▀", " ", "▐" }
M.border_chars_outer_thick = { "▛", "▀", "▜", "▐", "▟", "▄", "▙", "▌" }
M.border_chars_outer_thin = { "🭽", "▔", "🭾", "▕", "🭿", "▁", "🭼", "▏" }
M.border_chars_inner_thin = { " ", "▁", " ", "▏", " ", "▔", " ", "▕" }
M.border_chars_outer_thin_telescope = { "▔", "▕", "▁", "▏", "🭽", "🭾", "🭿", "🭼" }
M.border_chars_outer_thick_telescope = { "▀", "▐", "▄", "▌", "▛", "▜", "▟", "▙" }

M.bottom_thin = "▁"
M.top_thin = "▔"
M.left_thin = "▏"
M.right_thin = "▕"
M.left_thick = "▎"
M.right_thick = "🮇"
M.full_block = "█"
M.top_right_thin = "🭾"
M.top_left_thin = "🭽"
M.bottom_left_thin = "🭼"
M.bottom_right_thin = "🭿"
M.top_and_bottom = "🮀"

M.diagnostic_signs = {
  error = " ",
  warning = " ",
  info = " ",
  hint = "󱤅 ",
  other = "󰠠 ",
}

M.kind_icons = {
  Text = " ",
  Method = " ",
  Function = "󰊕 ",
  Constructor = " ",
  Field = " ",
  Variable = " ",
  Class = "󰠱 ",
  Interface = " ",
  Module = "󰏓 ",
  Property = " ",
  Unit = " ",
  Value = " ",
  Enum = " ",
  EnumMember = " ",
  Keyword = "󰌋 ",
  Snippet = "󰲋 ",
  Color = " ",
  File = " ",
  Reference = " ",
  Folder = " ",
  Constant = "󰏿 ",
  Struct = "󰠱 ",
  Event = " ",
  Operator = " ",
  TypeParameter = "󰘦 ",
  TabNine = "󰚩 ",
  Copilot = " ",
  Unknown = " ",
}

return M
