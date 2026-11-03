_hx_bind = function(o,m)
  if m == nil then return nil end;
  if _G.type(o) ~= 'table' then
    return function(...) return m(o, ...) end;
  end
  local f;
  if o._hx__closures == nil then
    _G.rawset(o, '_hx__closures', {});
  else
    f = o._hx__closures[m];
  end
  if (f == nil) then
    f = function(...) return m(o, ...) end;
    o._hx__closures[m] = f;
  end
  return f;
end
