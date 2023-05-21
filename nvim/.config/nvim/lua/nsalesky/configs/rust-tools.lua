local on_attach = require("lsp-zero").on_attach
local capabilities = require("lsp-zero").capabilities


return {
    server = {
        on_attach = on_attach,
        capabilities = capabilities,
    },
}
