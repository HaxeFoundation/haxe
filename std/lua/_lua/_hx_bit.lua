local _hx_bit
pcall(require, 'bit32') pcall(require, 'bit')
local _hx_bit_raw = bit or bit32

local function _hx_bit_clamp(v) return _hx_bit_raw.band(v, 2147483647 ) - _hx_bit_raw.band(v, 2147483648) end

if type(jit) == 'table' then
  _hx_bit = setmetatable({},{__index = function(t,k) return function(...) return _hx_bit_clamp(rawget(_hx_bit_raw,k)(...)) end end})
else
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end
end
