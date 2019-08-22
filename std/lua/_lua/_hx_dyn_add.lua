_hx_dyn_add = function(a,b)
  if (_G.type(a) == 'string' or _G.type(b) == 'string') then
    return Std.string(a)..Std.string(b)
  else
    return a + b;
  end;
end;
