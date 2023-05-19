vim.keymap.set("n", "<leader>pv", vim.cmd.Ex, { desc = "Open Ex" })

-- move selected lines up and down with J and K
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

