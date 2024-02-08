local hasBit32, bit32 = pcall(require, 'bit32')
if hasBit32 then --if we are on Lua 5.1, bit32 will be the default.
  _hx_bit_raw = bit32
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
  -- lua 5.2 weirdness
  _hx_bit.bnot = function(...) return _hx_bit_clamp(_hx_bit_raw.bnot(...)) end
  _hx_bit.bxor = function(...) return _hx_bit_clamp(_hx_bit_raw.bxor(...)) end
else
  --If we do not have bit32, fallback to 'bit'
  local hasBit, bit = pcall(require, 'bit')
  if not hasBit then
    error("Failed to load bit or bit32")
  end
  _hx_bit_raw = bit
  _hx_bit = setmetatable({}, { __index = _hx_bit_raw })
end

-- see https://github.com/HaxeFoundation/haxe/issues/8849
_hx_bit.bor = function(...) return _hx_bit_clamp(_hx_bit_raw.bor(...)) end
_hx_bit.band = function(...) return _hx_bit_clamp(_hx_bit_raw.band(...)) end
_hx_bit.arshift = function(...) return _hx_bit_clamp(_hx_bit_raw.arshift(...)) end
