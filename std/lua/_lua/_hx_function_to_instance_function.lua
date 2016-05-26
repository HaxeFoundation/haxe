local function _hx_functionToInstanceFunction(f)
  if type(f) == "function" then 
    return function(self,...) 
      return f(...) 
    end
  else 
    return f
  end
end
    
