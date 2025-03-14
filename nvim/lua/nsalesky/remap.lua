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
vim.keymap.set("x", "<leader>p", "\"_dP")

-- yank to system clipboard
vim.keymap.set("n", "<leader>y", "\"+y")
vim.keymap.set("v", "<leader>y", "\"+y")
vim.keymap.set("n", "<leader>Y", "\"+Y")

-- Swapping between buffers
vim.keymap.set("n", "[b", "<cmd>bprev<CR>", { desc = "Previous buffer" })
vim.keymap.set("n", "]b", "<cmd>bnext<CR>", { desc = "Next buffer" })

-- Swapping between tabs
vim.keymap.set("n", "[t", "<cmd>tabprev<CR>", { desc = "Previous tab" })
vim.keymap.set("n", "]t", "<cmd>tabnext<CR>", { desc = "Next tab" })

-- Moving between quickfix entries
vim.keymap.set("n", "[q", "<cmd>cprev<CR>", { desc = "Previous quickfix" })
vim.keymap.set("n", "]q", "<cmd>cnext<CR>", { desc = "Next quickfix" })

-- Jumping between diagnostics
vim.keymap.set("n", "[d", function()
  vim.diagnostic.goto_prev()
end, { desc = "Previous diagnostic" })
vim.keymap.set("n", "]d", function()
  vim.diagnostic.goto_next()
end, { desc = "Next diagnostic" })

-- Load diagnostics into location list
vim.keymap.set("n", "<leader>cq", function()
  vim.diagnostic.setloclist()
end, { desc = "Load diagnostics to loclist" })

-- Terminal mode bindings
vim.keymap.set("t", "<Esc>", [[<C-\><C-n>]]) -- exit terminal mode with Esc

-- Jump between buffers
vim.keymap.set("n", "gb", ":buffers<CR>:buffer<Space>", { desc = "Go to buffer" })

-- Lua development
vim.keymap.set("n", "<space><space>x", ":source %<CR>")
vim.keymap.set("n", "<space>x", ":.lua<CR>")
vim.keymap.set("v", "<space>x", ":lua<CR>")
