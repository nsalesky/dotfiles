-- Main body of the config inspired/taken from https://github.com/AlexvZyl/.dotfiles/blob/main/.config/nvim/lua/alex/ui/lualine.lua

-- Show Git diff status
local function diff_source()
  local gitsigns = vim.b.gitsigns_status_dict
  if gitsigns then
    return { added = gitsigns.added, modified = gitsigns.changed, removed = gitsigns.removed }
  end
end

-- Get the current buffer's filetype
local function get_current_filetype()
  return vim.api.nvim_buf_get_option(0, "filetype")
end

local function get_native_lsp()
  local buf_ft = get_current_filetype()
  local clients = vim.lsp.get_active_clients()
  if next(clients) == nil then
    return ""
  end

  local client_names = ""

  for _, client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
      if client_names == "" then
        client_names = client.name
      else
        client_names = client_names .. " " .. client.name
      end
    end
  end

  return client_names
end

local telescope = {
  sections = {
    lualine_a = {
      {
        "Telescope",
      },
    },
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {
      {
        "location",
      },
    },
  },
  filetypes = { "TelescopePrompt" },
}

return {
  options = {
    icons_enabled = true,
    theme = "auto",
    section_separators = " ",
    component_separators = " ",
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    },
  },
  sections = {
    lualine_a = {
      {
        "filename",
        file_status = true,
        path = 1,
        shorting_target = 40,
        symbols = {
          modified = "", -- Text to show when the file is modified.
          readonly = "", -- Text to show when the file is non-modifiable or readonly.
          unnamed = "No Name", -- Text to show for unnamed buffers.
          newfile = "", -- Text to show for newly created file before first write
        },
      },
    },
    lualine_b = {
      {
        "branch",
        icon = { "" },
      },
      {
        "diff",
        source = diff_source,
        symbols = { added = "+", modified = "~", removed = "-" },
      },
    },
    lualine_c = {
      {
        "diagnostics",
        sources = { "nvim_diagnostic" },
        colored = true,
      },
    },
    lualine_x = {
      {
        "encoding",
        padding = 0,
      },
      {
        get_current_filetype,
        padding = 0,
      },
    },
    lualine_y = {
      {
        get_native_lsp,
        icon = { " " },
      },
    },
    lualine_z = {
      {
        "location",
        padding = 1,
      },
      {
        "progress",
        padding = 1,
      },
    },
  },
  tabline = {
    lualine_a = {
      {
        "tabs",
        mode = 2,
      },
    },
  },
  winbar = {},
  inactive_winbar = {},
  extensions = {
    telescope,
    "nvim-tree",
  },
}
