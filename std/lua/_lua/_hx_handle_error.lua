function _hx_handle_error(obj)
  local message = tostring(obj)
  if _G.debug and _G.debug.traceback then
    message = _G.debug.traceback(message)
  end
  return setmetatable({}, { __tostring = function() return message end })
end
