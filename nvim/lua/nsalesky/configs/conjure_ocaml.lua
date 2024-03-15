local log = require("conjure.log")
local client = require("conjure.client")

local M = {
  ["buf-suffix"] = ".ml",
  ["comment-prefix"] = "(* ",
}

local state = client["new-state"]({ repl = nil })

local function with_repl_or_warn(f, opts)
  local repl = state("repl")
  if repl then
    f(repl)
  else
    log.append(M["comment-prefix"] .. "No REPL running")
  end
end

local function eval(opts)
  with_repl_or_warn(function (repl)
    repl.send(opts.code)
  end, {["batch?"] = true})
end

local function start()
  if state("repl") then
    log.append(M["comment-prefix"] .. "Can't start, REPL is already running.")
  else

  end
end

local function stop()

end

M["eval-str"] = eval()
M["on-load"] = start()
M["on-exit"] = stop()

return M
