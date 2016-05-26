local _hx_array_mt = {
  __newindex = function(t,k,v)
    if type(k) == 'number' and k >= t.length then
      t.length = k + 1
    end
    rawset(t,k,v)
  end
}

local function _hx_tabArray(tab,length)
  tab.length = length
  return setmetatable(tab, _hx_array_mt)
end
