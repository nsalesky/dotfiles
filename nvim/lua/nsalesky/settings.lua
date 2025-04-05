-- relative line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.hlsearch = false
vim.opt.incsearch = true

vim.opt.termguicolors = true

vim.opt.scrolloff = 8
vim.opt.signcolumn = "yes"
vim.opt.isfname:append("@-@")

vim.opt.updatetime = 50

vim.opt.colorcolumn = "120"

-- use system clipboard as the unnamed register for yank/paste
vim.opt.clipboard = "unnamedplus"

-- Set up Treesitter-based folding
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldenable = false

-- set up remote plugins
vim.g.python3_host_prog = vim.fn.expand("~/.config/nvim/.venv/bin/python3")

-- Settings used by Neovide
vim.opt.guifont = "JetBrainsMono Nerd Font:h15"
if vim.g.neovide then
  vim.g.neovide_hide_mouse_when_typing = true

  -- Catppuccin macchiato colors in terminal
  vim.g.terminal_color_0 = "#494d64"
  vim.g.terminal_color_1 = "#ed8796"
  vim.g.terminal_color_2 = "#a6da95"
  vim.g.terminal_color_3 = "#eed49f"
  vim.g.terminal_color_4 = "#8aadf4"
  vim.g.terminal_color_5 = "#f5bde6"
  vim.g.terminal_color_6 = "#8bd5ca"
  vim.g.terminal_color_7 = "#b8c0e0"
  vim.g.terminal_color_8 = "#5b6078"
  vim.g.terminal_color_9 = "#ed8796"
  vim.g.terminal_color_10 = "#a6da95"
  vim.g.terminal_color_11 = "#eed49f"
  vim.g.terminal_color_12 = "#8aadf4"
  vim.g.terminal_color_13 = "#f5bde6"
  vim.g.terminal_color_14 = "#8bd5ca"
  vim.g.terminal_color_15 = "#a5adcb"
end

-- Disable netrw
-- vim.g.loaded_netrw = 1
-- vim.g.loaded_netrwPlugin = 1

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
