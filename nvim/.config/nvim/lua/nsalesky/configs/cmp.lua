local cmp = require("cmp")
local luasnip = require("luasnip")
local utils = require("nsalesky.utils")

local function format(_, item)
    local MAX_LABEL_WIDTH = 55
    local function whitespace(max, len) return (' '):rep(max - len) end

    -- Limit content width.
    local content = item.abbr
    if #content > MAX_LABEL_WIDTH then
        item.abbr = vim.fn.strcharpart(content, 0, MAX_LABEL_WIDTH) .. '…'
    else
        item.abbr = content .. whitespace(MAX_LABEL_WIDTH, #content)
    end

    -- Replace kind with icons.
    item.kind = ' ' .. (utils.kind_icons[item.kind] or utils.kind_icons.Unknown) .. '│'

    -- Remove gibberish.
    item.menu = nil
    return item
end

local has_words_before = function()
    unpack = unpack or table.unpack
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

cmp.setup({
    mapping = cmp.mapping.preset.insert({
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-o>"] = cmp.mapping.complete(),
        ["<C-e>"] = cmp.mapping.abort(),
        ["<CR>"] = cmp.mapping.confirm({ select = true }),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
                -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable() 
                -- they way you will only jump inside the snippet region
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),

        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
    }),
    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "buffer" },
        { name = "crates" },
    }),
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    formatting = {
        fields = { "kind", "abbr" },
        format = format,
    },
    -- window = {
    --     completion = cmp.config.window.bordered({
    --         -- winhighlight = 'Normal:Pmenu,FloatBorder:PmenuBorder,CursorLine:PmenuSel,Search:None',
    --         scrollbar = true,
    --         border = utils.border_chars_outer_thin,
    --         col_offset = -1,
    --         side_padding = 0,
    --     }),
    --     documentation = cmp.config.window.bordered {
    --         -- winhighlight = 'Normal:Pmenu,FloatBorder:PmenuDocBorder,CursorLine:PmenuSel,Search:None',
    --         scrollbar = true,
    --         border = utils.border_chars_outer_thin,
    --         side_padding = 1, -- Not working?
    --     },
    -- }
})

