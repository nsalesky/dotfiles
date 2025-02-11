return {
  {
    "sindrets/diffview.nvim",
    cmd = {
      "DiffviewOpen",
    },
  },
  {
    "NeogitOrg/neogit",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {
      integrations = {
        diffview = true,
        telescope = true,
      },
      use_telescope = true,
    },
    keys = {
      {
        "<leader>gn",
        function()
          require("neogit").open()
        end,
        desc = "Neogit status",
      },
    },
  },
  -- {
  --   "ThePrimeagen/git-worktree.nvim",
  --   config = function()
  --     require("telescope").load_extension("git_worktree")
  --   end,
  --   keys = {
  --     {
  --       "<leader>gw",
  --       function()
  --         require("telescope").extensions.git_worktree.git_worktrees()
  --       end,
  --       desc = "Git worktrees",
  --     },
  --     {
  --       "<leader>gc",
  --       function()
  --         require("telescope").extensions.git_worktree.create_git_worktree()
  --       end,
  --       desc = "Create a worktree",
  --     },
  --   },
  -- },
  -- {
  --   "f-person/git-blame.nvim",
  --   event = "VeryLazy",
  --   opts = {
  --     message_template = " <summary> • <date> • <author> • <<sha>>",
  --     date_format = "%m-%d-%Y %H:%M:%S",
  --     virtual_text_column = 1,
  --   },
  -- },
}
