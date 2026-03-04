_hx_wrap_if_string_field_closure = function(o, fld)
  if _G.type(o) == 'string' then
    if fld == 'length' then
      return _G.string.len(o)
    else
      local m = String.prototype[fld]
      if m ~= nil then
        return function(...) return m(o, ...) end
      end
      return nil
    end
  else
    local v = o[fld]
    if _G.type(v) == 'function' then
      return _hx_bind(o, v)
    end
    return v
  end
end
