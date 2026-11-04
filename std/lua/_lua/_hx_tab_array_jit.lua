local _hx_hidden = {__id__=true, hx__closures=true, super=true, prototype=true, __fields__=true, __ifields__=true, __class__=true, __properties__=true, __mt__=true, __name__=true}

local ok, table_new = pcall(require, "table.new")
if not ok then table_new = function(narr, nrec) return {} end end

_hx_array_mt = {
    __newindex = function(t,k,v)
        local len = t.length
        t.length =  k >= len and (k + 1) or len
        rawset(t,k,v)
    end
}

function _hx_is_array(o)
    return type(o) == "table"
        and o.__enum__ == nil
        and getmetatable(o) == _hx_array_mt
end



function _hx_tab_array(tab, length)
    tab.length = length
    return setmetatable(tab, _hx_array_mt)
end

function _hx_tab_array_new(length)
    local tab = table_new(length, 1)
    tab.length = length
    return setmetatable(tab, _hx_array_mt)
end
