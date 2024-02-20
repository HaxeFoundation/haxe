local function _hx_mmt(prototype)
  return {
    __newindex = _hx_obj_newindex,
    __index = prototype,
    __tostring = _hx_tostring
  }
end

local function _hx_new(prototype)
  return setmetatable({ __fields__ = {} }, _hx_mmt(prototype))
end

local function _hx_nsh(metatable)
  return setmetatable({ __fields__ = {} }, metatable)
end
