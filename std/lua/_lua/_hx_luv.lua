if package.loaded.luv then
  _hx_luv = _G.require("luv");
else
  _hx_luv = {
    run=function(mode) return false end,
    loop_alive=function() return false end
  }
end