function _hx_handle_error(obj)
  local message = tostring(obj)
  if _G.debug and _G.debug.traceback then
    -- level 2 to skip _hx_handle_error
    message = _G.debug.traceback(message, 2)
  end
  return setmetatable({}, { __tostring = function() return message end })
end
