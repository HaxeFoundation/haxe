local function _hx_mmt(prototype)
  return {
    __newindex = _hx_obj_newindex,
    __index = prototype,
    __tostring = _hx_tostring
  }
end

local function _hx_nsh(metatbl)
  return setmetatable({ __fields__ = {} }, metatbl)
end
