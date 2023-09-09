-- Main body of the config inspired/taken from https://github.com/AlexvZyl/.dotfiles/blob/main/.config/nvim/lua/alex/ui/lualine.lua

local c = require("nsalesky.theme").get_lualine_colors()

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
                function()
                    return "Telescope"
                end,
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
                color = { fg = c.outer_pill_text },
            },
        },
        lualine_b = {
            {
                "branch",
                color = { fg = c.inner_pill_icon, bg = c.inner_pill_bg },
                icon = { "", color = { fg = c.inner_pill_icon } },
            },
            {
                "diff",
                source = diff_source,
                color = { fg = c.inner_pill_icon, bg = c.inner_pill_bg },
                symbols = { added = "+", modified = "~", removed = "-" },
                diff_color = {
                    added = { fg = "#67f771" },
                    modified = { fg = "#2fc0ed" },
                    removed = { fg = "#f93b58" },
                },
            },
        },
        lualine_c = {
            {
                "diagnostics",
                sources = { "nvim_diagnostic" },
                color = { fg = c.inner_fg, bg = c.inner_bg },
                -- symbols = { error = " ", warn = " ", info = " ", hint = "󱤅 ", other = "󰠠 " },
                -- diagnostics_color = {
                --     error = { fg = c.error },
                --     warn = { fg = c.warn },
                --     info = { fg = c.info },
                --     hint = { fg = c.hint },
                -- },
                colored = true,
            },
        },
        lualine_x = {
            {
                "encoding",
                color = { fg = c.inner_fg, bg = c.inner_bg },
                padding = 0,
            },
            {
                get_current_filetype,
                color = { fg = c.inner_fg, bg = c.inner_bg },
                padding = 0,
            },
        },
        lualine_y = {
            {
                get_native_lsp,
                color = { fg = c.inner_pill_text, bg = c.inner_pill_bg },
                icon = { " ", color = { fg = c.inner_pill_icon } },
            },
        },
        lualine_z = {
            {
                "location",
                color = { fg = c.outer_pill_text },
                padding = 1,
            },
            {
                "progress",
                color = { fg = c.outer_pill_text },
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
    },
}
