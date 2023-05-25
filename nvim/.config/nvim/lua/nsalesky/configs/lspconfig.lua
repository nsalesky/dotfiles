return {
    on_attach = function(_, bufnr)
        local opts = { buffer = bufnr, remap = false }

        vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end,
            vim.tbl_deep_extend("force", opts, { desc = "Go to Definition" }))
        vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end,
            vim.tbl_deep_extend("force", opts, { desc = "LSP Hover" }))
        vim.keymap.set("n", "<leader>cws", function() vim.lsp.buf.workspace_symbol() end, opts)
        vim.keymap.set("n", "<leader>cd", function() vim.diagnostic.open_float() end, opts)
        vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
        vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
        vim.keymap.set("n", "<leader>ca", function() vim.lsp.buf.code_action() end,
            vim.tbl_deep_extend("force", opts, { desc = "Code Actions" }))
        vim.keymap.set("n", "<leader>crr", function() vim.lsp.buf.references() end, opts)
        vim.keymap.set("n", "<leader>crn", function() vim.lsp.buf.rename() end,
            vim.tbl_deep_extend("force", opts, { desc = "Rename variable at point" }))
        vim.keymap.set("n", "<C-h>", function() vim.lsp.buf.signature_help() end,
            vim.tbl_deep_extend("force", opts, { desc = "LSP Signature Help" }))
    end,
    capabilities = require("cmp_nvim_lsp").default_capabilities(),
}
