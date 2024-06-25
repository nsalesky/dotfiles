require("luasnip.session.snippet_collection").clear_snippets "c"
require("luasnip.session.snippet_collection").clear_snippets "cpp"

local ls = require "luasnip"

local s = ls.snippet
local i = ls.insert_node

local fmt = require("luasnip.extras.fmt").fmt

-- TODO: get this working
local my_snippets = {
  s("#ifndefg", fmt([[
  #ifndef {a}
  #define {a}
  {}
  #endif
  ]], {
    a = i(1),
    i(0)
  }))
}

ls.add_snippets("c", my_snippets)
ls.add_snippets("cpp", my_snippets)

