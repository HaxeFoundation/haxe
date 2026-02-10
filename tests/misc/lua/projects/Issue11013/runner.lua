-- Block bit32 and bit libraries so _hx_bit_clamp falls through to native ops
local real_require = require
require = function(name)
    if name == "bit32" or name == "bit" then
        return nil
    end
    return real_require(name)
end

dofile("bin/test.lua")
