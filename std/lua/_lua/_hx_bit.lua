-- Try native Lua 5.3+ bitwise operators first
local _hx_bit_native = (function()
    local ok, fn = pcall(load, [[
        return {
            band = function(a, b) return a & b & 0xFFFFFFFF end,
            bor = function(a, b) return (a | b) & 0xFFFFFFFF end,
            bxor = function(a, b) return (a ~ b) & 0xFFFFFFFF end,
            bnot = function(a) return ~a & 0xFFFFFFFF end,
            lshift = function(a, b) return (a << b) & 0xFFFFFFFF end,
            rshift = function(a, b) return (a & 0xFFFFFFFF) >> b end,
            arshift = function(a, b) return (a // (1 << b)) & 0xFFFFFFFF end
        }
    ]])
    if ok and fn then return fn() end
    return nil
end)()

if _hx_bit_native then
  _hx_bit_raw = _hx_bit_native
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  -- native 64-bit operators need clamping to Int32 range
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end
  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end
  _hx_bit.bor = function(...) return _hx_bit_clamp(_hx_bit_raw.bor(...)) end
  _hx_bit.band = function(...) return _hx_bit_clamp(_hx_bit_raw.band(...)) end
  _hx_bit.arshift = function(...) return _hx_bit_clamp(_hx_bit_raw.arshift(...)) end
  _hx_bit.lshift = function(...) return _hx_bit_clamp(_hx_bit_raw.lshift(...)) end
elseif _G.bit32 or pcall(require, 'bit32') then
  -- lua 5.2 and 5.3 have bit32 builtin, otherwise it may be an external library
  _hx_bit_raw = _G.bit32 or require('bit32')
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  -- bit32 operations require manual clamping
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end
  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end
  -- see https://github.com/HaxeFoundation/haxe/issues/8849
  _hx_bit.bor = function(...) return _hx_bit_clamp(_hx_bit_raw.bor(...)) end
  _hx_bit.band = function(...) return _hx_bit_clamp(_hx_bit_raw.band(...)) end
  _hx_bit.arshift = function(...) return _hx_bit_clamp(_hx_bit_raw.arshift(...)) end
  _hx_bit.lshift = function(...) return _hx_bit_clamp(_hx_bit_raw.lshift(...)) end
elseif _G.bit or pcall(require, 'bit') then
  -- if we do not have bit32, fallback to bit, default on luajit
  _hx_bit_raw = _G.bit or require('bit')
  _hx_bit = _hx_bit_raw
else
  _hx_bit = setmetatable({}, {__index = function()
    error("Failed to load bit or bit32")
  end})
end
