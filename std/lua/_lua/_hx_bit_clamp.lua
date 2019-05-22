_hx_bit_clamp = function(v)
  if v <= 2147483647 and v >= -2147483648 then
    if v > 0 then return _G.math.floor(v)
    else return _G.math.ceil(v)
    end
  end
  if v > 2251798999999999 then v = v*2 end;
  if (v ~= v or math.abs(v) == _G.math.huge) then return nil end
  return _hx_bit.band(v, 2147483647 ) - math.abs(_hx_bit.band(v, 2147483648))
end
