return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "saadparwaiz1/cmp_luasnip",
      "petertriho/cmp-git",
      "luckasRanarison/tailwind-tools.nvim",
    },
    config = function()
      require("nsalesky.configs.cmp")
    end,
  },
  "hrsh7th/cmp-nvim-lsp",
  {
    "L3MON4D3/LuaSnip",
    version = "1",
    build = "make install_jsregexp", -- optional
    dependencies = {
      "rafamadriz/friendly-snippets", -- adds a bunch of preconfigured snippets
    },
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
    end,
  },
}
