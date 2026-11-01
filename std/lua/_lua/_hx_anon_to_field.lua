_hx_anonToField = function(f)
  if type(f) == 'function' then
    return function(...)
      return f(nil, ...)
    end
  else
    return f
  end
end
