return {
  -- {
  --   "nsalesky/runnables.nvim",
  --   dir = "~/Projects/neovim/runnables.nvim",
  --   lazy = false,
  --   opts = {},
  -- },
  {
    "axelvc/template-string.nvim",
    ft = { "html", "typescript", "javascript", "typescriptreact", "javascriptreact", "vue", "svelte", "python", "cs" },
    opts = {
      filetypes = {
        "html",
        "typescript",
        "javascript",
        "typescriptreact",
        "javascriptreact",
        "vue",
        "svelte",
        "python",
        "cs",
      },
      remove_template_string = true,
      restore_quotes = {
        normal = [[']],
        python = [[']],
        jsx = [["]],
      },
    },
  },
}
