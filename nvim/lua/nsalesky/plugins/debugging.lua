return {
  {
    "mfussenegger/nvim-dap",
    keys = {
      -- { "<leader>db", ":DapToggleBreakpoint<CR>", desc = "Toggle breakpoint" },
      { "<leader>do", ":DapStepOver <CR>", desc = "Step Over" },
      { "<leader>di", ":DapStepInto <CR>", desc = "Step Into" },
      { "<leader>de", ":DapStepOut <CR>", desc = "Step Out" },
      { "<leader>dc", ":DapContinue <CR>", desc = "Continue Execution" },
      { "<leader>dt", ":DapTerminate <CR>", desc = "Terminate Execution" },
    },
    config = function()
      local dap = require("dap")
      vim.fn.sign_define("DapBreakpoint", { text = "" })

      -- Set up debug adapters
      dap.adapters.gdb = {
        type = "executable",
        command = "gdb",
        args = { "-i", "dap" },
      }

      -- Set up debug configurations
      dap.configurations.c = {
        {
          name = "Launch (gdb)",
          type = "gdb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopAtBeginningOfMainSubprogram = false,
        },
      }
    end,
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio" },
    config = function()
      local dapui = require("dapui")
      local dap = require("dap")

      dapui.setup({
        layouts = {
          {
            elements = {
              { id = "watches", size = 0.25 },
              { id = "watches", size = 0.25 },
            },
            position = "left",
            size = 40,
          },
        },
      })

      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end

      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end

      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end
    end,
  },
  {
    "theHamsta/nvim-dap-virtual-text",
    dependencies = { "mfussenegger/nvim-dap" },
    opts = {},
  },
  {
    "Weissle/persistent-breakpoints.nvim",
    event = "BufReadPost",
    opts = {
      load_breakpoints_event = { "BufReadPost" },
    },
    keys = {
      {
        "<leader>db",
        function()
          require("persistent-breakpoints.api").toggle_breakpoint()
        end,
        { noremap = true, silent = true },
      },
    },
  },
}
