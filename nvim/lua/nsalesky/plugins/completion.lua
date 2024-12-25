return {
  {
    "Saghen/blink.cmp",
    lazy = false,
    version = "v0.*",
    dependencies = {
      { "L3MON4D3/LuaSnip", version = "v2.*" },
    },
    opts = {
      appearance = {
        use_nvim_cmp_as_default = true,
        nerd_font_variant = "mono",
      },
      sources = {
        default = { "lsp", "path", "luasnip", "buffer" },
        cmdline = {},
      },
      signature = { enabled = true },
      completion = {
        documentation = {
          auto_show = false,
          auto_show_delay_ms = 150,
        },
      },
      keymap = {
        ["<C-space>"] = { "show", "show_documentation", "hide_documentation" },
        ["<C-e>"] = { "hide" },
        ["<C-y>"] = { "select_and_accept" },

        ["<C-p>"] = { "select_prev", "fallback" },
        ["<C-n>"] = { "select_next", "fallback" },

        ["<C-b>"] = { "scroll_documentation_up", "fallback" },
        ["<C-f>"] = { "scroll_documentation_down", "fallback" },

        ["<C-k>"] = { "snippet_forward", "fallback" },
        ["<C-j>"] = { "snippet_backward", "fallback" },
      },
      snippets = {
        expand = function(snippet)
          require("luasnip").lsp_expand(snippet)
        end,
        active = function(filter)
          if filter and filter.direction then
            return require("luasnip").jumpable(filter.direction)
          end
          return require("luasnip").in_snippet()
        end,
        jump = function(direction)
          require("luasnip").jump(direction)
        end,
      },
    },
    opts_extend = {
      "sources.default",
    },
  },
}
