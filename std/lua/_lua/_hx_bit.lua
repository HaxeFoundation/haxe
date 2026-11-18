-- Try native Lua 5.3+ bitwise operators first
local _hx_bit_native = (function()
    local ok, fn = pcall(load, [[
        -- mask to unsigned 32-bit, then convert to signed Int32
        local function _s(v)
            v = v & 0xFFFFFFFF
            if v >= 0x80000000 then v = v - 0x100000000 end
            return v
        end
        return {
            band = function(a, b) return _s(a & b) end,
            bor = function(a, b) return _s(a | b) end,
            bxor = function(a, b) return _s(a ~ b) end,
            bnot = function(a) return _s(~a) end,
            lshift = function(a, b) return _s(a << b) end,
            rshift = function(a, b) return (a & 0xFFFFFFFF) >> b end,
            arshift = function(a, b) return _s(a // (1 << b)) end
        }
    ]])
    if ok and fn then return fn() end
    return nil
end)()

if _hx_bit_native then
  _hx_bit_raw = _hx_bit_native
  _hx_bit = _hx_bit_raw
elseif _G.bit32 or pcall(require, 'bit32') then
  -- lua 5.2 has bit32 builtin, otherwise it may be an external library
  _hx_bit_raw = _G.bit32 or require('bit32')
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  -- bit32 produces uint32 outputs. convert to signed
  local function wrap(f)
    return function(...)
      local v = f(...)
      if v >= 0x80000000 then v = v - 0x100000000 end
      return v
    end
  end
  -- leave _hx_bit.rshift unwrapped because Haxe specifies it as unsigned anyway
  _hx_bit.bnot = wrap(_hx_bit_raw.bnot)
  _hx_bit.bxor = wrap(_hx_bit_raw.bxor)
  _hx_bit.bor = wrap(_hx_bit_raw.bor)
  _hx_bit.band = wrap(_hx_bit_raw.band)
  _hx_bit.arshift = wrap(_hx_bit_raw.arshift)
  _hx_bit.lshift = wrap(_hx_bit_raw.lshift)
elseif _G.bit or pcall(require, 'bit') then
  -- if we do not have bit32, fallback to bit, default on luajit
  _hx_bit_raw = _G.bit or require('bit')
  _hx_bit = _hx_bit_raw
else
  _hx_bit = setmetatable({}, {__index = function()
    error("Bitwise operation unsupported: none of bit, bit32, or native operators could be used")
  end})
end
