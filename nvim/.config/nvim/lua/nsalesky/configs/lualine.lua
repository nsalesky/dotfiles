-- Main body of the config inspired/taken from https://github.com/AlexvZyl/.dotfiles/blob/main/.config/nvim/lua/alex/ui/lualine.lua

local c = require("catppuccin.palettes").get_palette()

-- Show Git diff status
-- local function diff_source()
--     local gitsigns = vim.b.gitsigns_status_dict
--     if gitsigns then
--         return { added = gitsigns.added, modified = gitsigns.changed, removed = gitsigns.removed }
--     end
-- end

-- Get the current buffer's filetype
local function get_current_filetype()
    return vim.api.nvim_buf_get_option(0, "filetype")
end

-- Get the current buffer's type
local function get_current_buftype()
    return vim.api.nvim_buf_get_option(0, "buftype")
end

local function get_native_lsp()
    local buf_ft = get_current_filetype()
    local clients = vim.lsp.get_active_clients()
    if next(clients) == nil then
        return ""
    end
    for _, client in ipairs(clients) do
        local filetypes = client.config.filetypes
        if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
            return client.name
        end
    end
end

-- Display the difference in commits between local and head
local Job = require("plenary.job")
local function get_git_compare()
    -- Get the path of the current directory
    local curr_dir = vim.api.nvim_buf_get_name(0):match("(.*" .. "/" .. ")")

    -- Run job to fetch git info
    local result = Job:new({
            command = "git",
            cwd = curr_dir,
            args = { "rev-list", "--left-right", "--count", "HEAD...@{upstream}" },
        })
        :sync(100)[1]

    -- Process the result
    if type(result) ~= "string" then
        return ""
    end
    local ok, ahead, behind = pcall(string.match, result, "(%d+)%s*(%d+)")
    if not ok then
        return ""
    end

    -- No file, so no git
    if get_current_buftype() == "nofile" then
        return ""
    end

    return "󰁅 " .. behind .. " 󰁝 " .. ahead
end

local function telescope_text()
    return "Telescope"
end

local telescope = {
    sections = {
        lualine_a = {
            {
                "mode",
                icon = { "" },
                separator = { right = " ", left = "" },
            },
        },
        lualine_b = {
            {
                telescope_text,
                icon = { "  " },
                color = { fg = c.text, bg = c.surface1 },
            },
        },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {
            {
                "location",
                icon = { "", align = "left" },
            },
        },
    },
    filetypes = { "TelescopePrompt" },
}

return {
    options = {
        icons_enabled = true,
        theme = "catppuccin",
        section_separators = { left = " ", right = " " },
        component_separators = { left = "", right = "" },
        disabled_filetypes = {
            statusline = {},
            winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        globalstatus = true,
        refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
        },
    },
    sections = {
        lualine_a = {
            {
                "mode",
                icon = { "" },
                separator = { right = " ", left = "" },
            },
        },
        lualine_b = {
            {
                "filetype",
                colored = false,
                icon_only = true,
                color = { fg = c.mauve, bg = c.surface1 },
            },
            {
                "filename",
                color = { fg = c.text, bg = c.surface1 },
                separator = " ",
                padding = 0,

                file_status = true,
                path = 4,
                shorting_target = 40,
                symbols = {
                    modified = "[+]",      -- Text to show when the file is modified.
                    readonly = "[-]",      -- Text to show when the file is non-modifiable or readonly.
                    unnamed = "[No Name]", -- Text to show for unnamed buffers.
                    newfile = "[New]",     -- Text to show for newly created file before first write
                },
            },
        },
        lualine_c = {
            {
                "branch",
                icon = { "", color = { fg = c.mauve } },
                separator = " ",
                padding = 0,
            },
            {
                get_git_compare,
                icon = { "", color = { fg = c.mauve } },
                separator = " ",
                padding = 0,
            },
            {
                "diff",
                -- source = diff_source,
                padding = 0,
                icon = { " " },
                symbols = { added = " ", modified = " ", removed = " " },
                diff_color = {
                    added = { fg = c.surface1 },
                    modified = { fg = c.surface1 },
                    removed = { fg = c.surface1 },
                },
            },
        },
        lualine_x = {
            {
                "diagnostics",
                sources = { "nvim_diagnostic" },
                -- symbols = { error = " ", warn = " ", info = " ", hint = "󱤅 ", other = "󰠠 " },
                -- diagnostics_color = {
                --     error = { fg = c.error },
                --     warn = { fg = c.warn },
                --     info = { fg = c.info },
                --     hint = { fg = c.hint },
                -- },
                colored = true,
                padding = 1,
            },
        },
        lualine_y = {
            {
                get_native_lsp,
                padding = 2,
                separator = " ",
                color = { fg = c.text, bg = c.surface1 },
                icon = { " ", color = { fg = c.mauve } },
            },
        },
        lualine_z = {
            {
                "location",
                icon = { "", align = "left" },
            },
            {
                "progress",
                icon = { "", align = "left" },
                separator = { right = "", left = "" },
            },
        },
    },
    tabline = {},
    winbar = {},
    inactive_winbar = {},
    extensions = {
        telescope,
    },
}
