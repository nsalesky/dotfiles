local cmp = require("cmp")
local luasnip = require("luasnip")
local utils = require("nsalesky.utils")

local function format(entry, item)
  local MAX_LABEL_WIDTH = 55
  local function whitespace(max, len)
    return (" "):rep(max - len)
  end

  if entry.source.name == "vim-dadbod-completion" then
    item.kind = "Field"
  end

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

cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<C-e>"] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "buffer" },
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
