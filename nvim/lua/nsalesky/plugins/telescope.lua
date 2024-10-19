return {
  "nvim-telescope/telescope.nvim",
  branch = "0.1.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope-live-grep-args.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  config = function()
    local builtin = require("telescope.builtin")

    local function find_file()
      builtin.find_files({
        hidden = true,
      })
    end

    vim.keymap.set("n", "<leader>ff", find_file, { desc = "File" })
    vim.keymap.set("n", "<C-p>", find_file, { desc = "Find File" })

    vim.keymap.set("n", "<leader>fb", builtin.buffers, { desc = "Buffers" })
    vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "Diagnostics" })
    vim.keymap.set("n", "<leader>sc", builtin.colorscheme, { desc = "Color Scheme" })

    vim.keymap.set("n", "<leader>fs", function()
      require("telescope").extensions.live_grep_args.live_grep_args()
    end, { desc = "Search For Text" })

    local telescope = require("telescope")
    local lga_actions = require("telescope-live-grep-args.actions")
    local open_with_trouble = require("trouble.sources.telescope").open

    -- Use this to add more results without clearing the trouble list
    -- local add_to_trouble = require("trouble.sources.telescope").add

    telescope.setup({
      defaults = {
        mappings = {
          i = { ["<c-t>"] = open_with_trouble },
          n = { ["<c-t>"] = open_with_trouble },
        },
      },
      extensions = {
        live_grep_args = {
          auto_quoting = true,
          mappings = {
            i = {
              ["<c-k>"] = lga_actions.quote_prompt(),
              ["<c-i>"] = lga_actions.quote_prompt({ postfix = " --iglob " }),
              ["<c-t>"] = open_with_trouble,
            },
            n = { ["<c-t>"] = open_with_trouble },
          },
        },
        ["ui-select"] = {
          require("telescope.themes").get_dropdown({}),
        },
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
      },
    })

    require("telescope").load_extension("ui-select")
    require("telescope").load_extension("fzf")
  end,
}
