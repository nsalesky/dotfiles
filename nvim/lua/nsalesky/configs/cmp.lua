local cmp = require("cmp")
local luasnip = require("luasnip")
local utils = require("nsalesky.utils")
-- local tailwind_utils = require("tailwind-tools.utils")

local function format(entry, item)
  local MAX_LABEL_WIDTH = 55
  local function whitespace(max, len)
    return (" "):rep(max - len)
  end

  -- local doc = entry.completion_item.documenttion
  if entry.source.name == "vim-dadbod-completion" then
    item.kind = "Field"
  end
  -- elseif entry.kind == "Color" and type(doc) == "string" then
  --   local _, _, r, g, b = doc:find("rgba?%((%d+), (%d+), (%d+)")
  --   if r then item.kind_hl_group = tailwind_utils.set_hl_from(r, g, b, "foreground") end
  -- end

  -- Limit content width.
  local content = item.abbr
  if #content > MAX_LABEL_WIDTH then
    item.abbr = vim.fn.strcharpart(content, 0, MAX_LABEL_WIDTH) .. "…"
  else
    item.abbr = content .. whitespace(MAX_LABEL_WIDTH, #content)
  end

  -- Replace kind with icons.
  item.kind = " " .. (utils.kind_icons[item.kind] or utils.kind_icons.Unknown) .. "│"

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
  -- completion = {
  --   autocomplete = false,
  -- },
  mapping = cmp.mapping.preset.insert({
    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<C-e>"] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
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
    { name = "conjure" },
    { name = "nvim_lsp_signature_help" },
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
})

cmp.setup.filetype("gitcommit", {
  sources = cmp.config.sources({
    { name = "git" },
  }, {
    { name = "buffer" },
  }),
})
