return {
  {
    "mfussenegger/nvim-dap-python",
    ft = "python",
    dependencies = {
      "mfussenegger/nvim-dap",
      "rcarriga/nvim-dap-ui",
    },
    config = function(_, _)
      local path = "~/.local/share/nvim/mason/packages/debugpy/venv/bin/python"
      require("dap-python").setup(path, { include_configs = false })

      local dap = require("dap")
      dap.configurations.python = {
        {
          type = "python",
          request = "attach",
          name = "Attach remote",
          connect = function()
            local host = vim.fn.input("Host [127.0.0.1]: ")
            host = host ~= "" and host or "127.0.0.1"
            local port = tonumber(vim.fn.input("Port [5678]: ")) or 5678

            return { host = host, port = port }
          end,
          pathMappings = function()
            return {
              {
                localRoot = vim.fn.getcwd(),
                remoteRoot = vim.fn.input("Remote root: "),
              },
            }
          end,
        },
      }
    end,
    keys = {
      {
        "<leader>dpr",
        function()
          require("dap-python").test_method()
        end,
        desc = "Test method",
      },
    },
  },
}
