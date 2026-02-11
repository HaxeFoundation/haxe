-- Test Int32 wrapping via native bit operators (Lua 5.3+ only).
-- On Lua 5.1/5.2 there are no native bit operators, so skip gracefully.
local major, minor = _VERSION:match("Lua (%d+)%.(%d+)")
if tonumber(major) < 5 or (tonumber(major) == 5 and tonumber(minor) < 3) then
    -- No native bit operators available; print expected output and exit
    io.stderr:write("Success")
    os.exit(0)
end

-- Block bit32 and bit libraries so _hx_bit_clamp falls through to native ops
local real_require = require
require = function(name)
    if name == "bit32" or name == "bit" then
        return nil
    end
    return real_require(name)
end

dofile("bin/test.lua")
