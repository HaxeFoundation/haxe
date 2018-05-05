local function _hx_apply(f, ...)
  return f(...)
end
local function _hx_apply_self(self, f, ...)
  return self[f](self,...)
end
