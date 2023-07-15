return {
    "echasnovski/mini.nvim",
    version = false,
    config = function()
        require("mini.files").setup({
            options = {
                use_as_default_explorer = true,
            },
        })
    end,
}
