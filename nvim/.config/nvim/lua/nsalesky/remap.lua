vim.keymap.set("n", ";", ":")

vim.keymap.set("n", "<leader>fv", vim.cmd.Ex, { desc = "Open Ex" })

-- move selected lines up and down with J and K
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- keep the cursor in the midddle of the screen when jumping with C-u and C-d
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- keep the cursor in the middle of the screen for search terms
vim.keymap.set("n", "n", "nzzzv")
vim.keymap.set("n", "N", "Nzzzv")

-- don't override register when pasting over a selection
vim.keymap.set("x", "<leader>p", '"_dP')

-- yank to system clipboard
vim.keymap.set("n", "<leader>y", '"+y')
vim.keymap.set("v", "<leader>y", '"+y')
vim.keymap.set("n", "<leader>Y", '"+Y')

-- switch to previous tmux session
-- vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")
