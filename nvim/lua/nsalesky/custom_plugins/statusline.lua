local M = {}
local NickStatusline = {}

-- Determines if the window width is less than the provided `width`.
-- Returns true if the section should be truncated and false otherwise.
---@param width integer
---@return boolean
local function is_truncated(width)
  local win_width = vim.api.nvim_win_get_width(0)

  return width > win_width
end

---@alias __statusline_args table Section arguments.
---@alias __statusline_section string Section string.-

-- Sections

local CTRL_S = vim.api.nvim_replace_termcodes("<C-S>", true, true, true)
local CTRL_V = vim.api.nvim_replace_termcodes("<C-V>", true, true, true)
-- stylua: ignore start
M.modes = setmetatable({
  ["n"]      = { long = "Normal", short = "N", hl = "NickStatuslineModeNormal" },
  ["v"]      = { long = "Visual", short = "V", hl = "NickStatuslineModeVisual" },
  ["V"]      = { long = "V-Line", short = "V-L", hl = "NickStatuslineModeVisual" },
  [CTRL_V]   = { long = "V-Block", short = "V-B", hl = "NickStatuslineModeVisual" },
  ["s"]      = { long = "Select", short = "S", hl = "NickStatuslineModeVisual" },
  ["S"]      = { long = "S-Line", short = "S-L", hl = "NickStatuslineModeVisual" },
  [CTRL_S]   = { long = "S-Block", short = "S-B", hl = "NickStatuslineModeVisual" },
  ["i"]      = { long = "Insert", short = "I", hl = "NickStatuslineModeInsert" },
  ["R"]      = { long = "Replace", short = "R", hl = "NickStatuslineModeReplace" },
  ["c"]      = { long = "Command", short = "C", hl = "NickStatuslineModeCommand" },
  ["r"]      = { long = "Prompt", short = "P", hl = "NickStatuslineModeOther" },
  ["!"]      = { long = "Shell", short = "S", hl = "NickStatuslineModeOther" },
  ["t"]      = { long = "Terminal", short = "T", hl = "NickStatuslineModeOther" },
}, {
  -- Return Unknown by default
  __index = function()
    return     { long = "Unknown", short = "UNK", hl = "NickStatuslineModeOther" }
  end,
})
-- stylua: ignore end

local function section_mode()
  local mode = vim.api.nvim_get_mode().mode
  local mode_info = M.modes[mode]

  if is_truncated(75) then
    return ("%%#%s#%s "):format(mode_info.hl, mode_info.short)
  else
    return ("%%#%s#%s "):format(mode_info.hl, mode_info.long)
  end
end

---@return __statusline_section
local function section_branch()
  local git_data = vim.b.minigit_summary_string
  if git_data == nil then
    return ""
  end

  return ("%s"):format(git_data)
end

---@return __statusline_section
local function section_diff()
  if is_truncated(75) then
    return ""
  end

  local summary = vim.b.minidiff_summary
  if summary == nil then
    return ""
  end

  -- local icon = args.icon or ""
  local s = "%#NickStatuslineDevInfo#"

  if summary.add then
    s = s .. " %#NickStatuslineDiffAdd#" .. summary.add
  end

  if summary.change then
    s = s .. " %#NickStatuslineDiffChange#" .. summary.change
  end

  if summary.delete then
    s = s .. " %#NickStatuslineDiffDelete#" .. summary.delete
  end

  return s
end

---@return __statusline_section
local function section_filename()
  local filename = vim.api.nvim_buf_get_name(0)
  local relative_filename = vim.fn.fnamemodify(filename, ":.")
  return ("%%#NickStatuslineFilename# %s"):format(relative_filename)
end

---@param args __statusline_args
---
---@return __statusline_section
local function section_lsp(args)
  if is_truncated(75) then
    return ""
  end

  local buf_ft = vim.api.nvim_get_option_value("filetype", {})
  local clients = vim.lsp.get_clients()
  if next(clients) == nil then
    return ""
  end

  local client_names = "%#MiniStatuslineDevinfo#"

  for _, client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
      if client_names == "" then
        client_names = client_names .. client.name
      else
        client_names = client_names .. " " .. client.name
      end
    end
  end

  return client_names
end

function NickStatusline.active()
  local s = ""
  s = s .. section_mode()
  s = s .. section_branch()
  s = s .. section_diff()
  s = s .. section_filename()

  return s
end

function NickStatusline.inactive()
  return ""
end

function M.create_hl_groups()
  vim.api.nvim_set_hl(0, "NickStatuslineDevInfo", { link = "MiniStatuslineDevinfo" })
  vim.api.nvim_set_hl(0, "NickStatuslineFilename", { link = "StatusLineNC" })
  vim.api.nvim_set_hl(0, "NickStatuslineFileinfo", { link = "StatusLine" })
  vim.api.nvim_set_hl(0, "NickStatuslineInactive", { link = "StatusLineNC" })

  vim.api.nvim_set_hl(0, "NickStatuslineDiffAdd", { fg = "#a6da95", bg = "#494d64" })
  vim.api.nvim_set_hl(0, "NickStatuslineDiffChange", { fg = "#eed49f", bg = "#494d64" })
  vim.api.nvim_set_hl(0, "NickStatuslineDiffDelete", { fg = "#ed8796", bg = "#494d64" })

  vim.api.nvim_set_hl(0, "NickStatuslineModeNormal", { link = "MiniStatuslineModeNormal" })
  vim.api.nvim_set_hl(0, "NickStatuslineModeInsert", { link = "MiniStatuslineModeInsert" })
  vim.api.nvim_set_hl(0, "NickStatuslineModeVisual", { link = "MiniStatuslineModeVisual" })
  vim.api.nvim_set_hl(0, "NickStatuslineModeReplace", { link = "MiniStatuslineModeReplace" })
  vim.api.nvim_set_hl(0, "NickStatuslineModeCommand", { link = "MiniStatuslineModeCommand" })
  vim.api.nvim_set_hl(0, "NickStatuslineModeOther", { link = "MiniStatuslineModeOther" })
end

function M.create_autocommands()
  local augroup = vim.api.nvim_create_augroup("NickStatusline", {})

  local au = function(event, pattern, callback, desc)
    vim.api.nvim_create_autocmd(event, { group = augroup, pattern = pattern, callback = callback, desc = desc })
  end

  au({ "WinEnter", "BufWinEnter" }, "*", M.ensure_content, "Ensure statusline content")
end

M.ensure_content = vim.schedule_wrap(function()
  local cur_win_id = vim.api.nvim_get_current_win()
  for _, win_id in ipairs(vim.api.nvim_list_wins()) do
    vim.wo[win_id].statusline = (win_id == cur_win_id) and "%{%v:lua.NickStatusline.active()%}"
      or "%{%v:lua.NickStatusline.inactive()%}"
  end
end)

function NickStatusline.setup()
  -- Export the module as a global
  _G.NickStatusline = NickStatusline

  M.create_autocommands()
  M.create_hl_groups()

  M.ensure_content()
  vim.o.laststatus = 2
  vim.go.statusline = "%{%v:lua.NickStatusline.active()%}"
end

NickStatusline.setup()

return NickStatusline
