-- require this for lua 5.1
pcall(require, 'bit')
if bit then
  _hx_bit_raw = bit
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw });
else
  _hx_bit_raw = _G.require('bit32')
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw });
  -- lua 5.2 weirdness
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end;
  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end;
end
-- see https://github.com/HaxeFoundation/haxe/issues/8849
_hx_bit.bor = function(...) return _hx_bit_clamp(_hx_bit_raw.bor(...)) end;
_hx_bit.band = function(...) return _hx_bit_clamp(_hx_bit_raw.band(...)) end;
_hx_bit.arshift = function(...) return _hx_bit_clamp(_hx_bit_raw.arshift(...)) end;
