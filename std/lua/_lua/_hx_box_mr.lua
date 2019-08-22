_hx_box_mr = function(x,nt)
    res = _hx_o({__fields__={}})
    for i,v in ipairs(nt) do
      res[v] = x[i]
    end
    return res
end
