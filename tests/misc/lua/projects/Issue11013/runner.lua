-- Test Int32 wrapping without bit32/bit libraries.
-- On Lua 5.3+ this exercises native bit operators.
-- On Lua 5.1/5.2 this exercises the modulo-based fallback.

-- Block bit32 and bit libraries so _hx_bit_clamp uses native ops or modulo fallback
local real_require = require
require = function(name)
    if name == "bit32" or name == "bit" then
        return nil
    end
    return real_require(name)
end

dofile("bin/test.lua")
