local function _hx_staticToInstance(tab)
  return setmetatable({}, {
    __index = function(t,k)
      if type(rawget(tab,k)) == 'function' then 
	return function(self,...)
	  return rawget(tab,k)(...)
	end
      else
	return rawget(tab,k)
      end
    end
  })
end
