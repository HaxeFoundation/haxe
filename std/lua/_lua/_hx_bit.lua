local _hx_bit
pcall(require, 'bit32') pcall(require, 'bit')
if type(jit) == 'table' then
  local _hx_bit_raw = bit or bit32
  local function _hx_bitfix(v) return (v >= 0) and v or (4294967296 + v) end
  _hx_bit = setmetatable({},{__index = function(t,k) return function(...) return _hx_bitfix(rawget(_hx_bit_raw,k)(...)) end end})
else
  _hx_bit = bit or bit32
end
