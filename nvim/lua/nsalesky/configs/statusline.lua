local mini_status = require("mini.statusline")

local M = {}

local function create_hl_groups()
  vim.api.nvim_set_hl(0, "NickStatuslineDiffAdd", { fg = "#a6da95", bg = "#494d64" })
  vim.api.nvim_set_hl(0, "NickStatuslineDiffChange", { fg = "#eed49f", bg = "#494d64" })
  vim.api.nvim_set_hl(0, "NickStatuslineDiffDelete", { fg = "#ed8796", bg = "#494d64" })
end

function M.setup()
  create_hl_groups()
end

---@alias __statusline_args table Section arguments.
---@alias __statusline_section string Section string.-

---@param args __statusline_args
---
---@return __statusline_section
function M.section_diff(args)
  if mini_status.is_truncated(args.trunc_width) then
    return ""
  end

  local summary = vim.b.minidiff_summary
  if summary == nil then
    return ""
  end

  -- local icon = args.icon or ""
  local s = ""

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

---@param args __statusline_args
---
---@return __statusline_section
function M.section_lsp(args)
  if mini_status.is_truncated(args.trunc_width) then
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

return M
