-- Useful for plugin development
function P(v)
    print(vim.inspect(v))
end

function RELOAD(...)
    return require("plenary.reload").reload_module(...)
end

function R(name)
    RELOAD(name)
    return require(name)
end
