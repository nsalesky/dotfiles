return {
    "epwalsh/obsidian.nvim",
    lazy = true,
    event = { "BufReadPre /home/nsalesky/Documents/obsidian/**.md" },
    opts = {
        dir = "/home/nsalesky/Documents/obsidian",
        completion = {
            nvim_cmp = true,
        },

        -- templates = {
        --     subdir = "templates",
        -- },

        follow_url_func = function(url)
            -- vim.fn.jobstart({"open", url}) -- Mac OS
            vim.fn.jobstart({ "xdg-open", url }) -- Linux
        end,

        finder = "telescope.nvim",
    },
}
