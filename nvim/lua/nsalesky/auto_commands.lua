local Job = require("plenary.job")

local autocmd_group = vim.api.nvim_create_augroup("Custom auto-commands", { clear = true })

-- Automatically build LaTeX documents on save if a build action exists
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  pattern = { "*.tex" },
  desc = "Auto-build LaTeX documents on save if a build action exists",
  callback = function()
    Job:new({
      command = "just",
      args = { "build" },
      cwd = vim.fn.getcwd(),
    }):start()
  end,
  group = autocmd_group,
})
