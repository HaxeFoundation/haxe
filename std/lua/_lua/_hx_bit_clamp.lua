if _hx_bit_raw then
    _hx_bit_clamp = function(v)
    if v <= 2147483647 and v >= -2147483648 then
        if v > 0 then return _G.math.floor(v)
        else return _G.math.ceil(v)
        end
    end
    if v > 2251798999999999 then v = v*2 end;
    if (v ~= v or math.abs(v) == _G.math.huge) then return nil end
    return _hx_bit_raw.band(v, 2147483647 ) - math.abs(_hx_bit_raw.band(v, 2147483648))
    end
else
    -- Try to use native Lua 5.3+ bit operators for proper 32-bit wrapping
    local native_bit_clamp = (function()
        local ok, fn = pcall(load, [[
            return function(v)
                if v <= 2147483647 and v >= -2147483648 then
                    if v > 0 then return _G.math.floor(v)
                    else return _G.math.ceil(v)
                    end
                end
                if v > 2251798999999999 then v = v*2 end
                if (v ~= v or math.abs(v) == _G.math.huge) then return nil end
                return (v & 0x7FFFFFFF) - (v & 0x80000000)
            end
        ]])
        if ok and fn then return fn() end
        return nil
    end)()

    if native_bit_clamp then
        _hx_bit_clamp = native_bit_clamp
    else
        -- Fallback for Lua 5.1/5.2 without bit library: clamp instead of wrap
        _hx_bit_clamp = function(v)
            if v < -2147483648 then
                return -2147483648
            elseif v > 2147483647 then
                return 2147483647
            elseif v > 0 then
                return _G.math.floor(v)
            else
                return _G.math.ceil(v)
            end
        end
    end
end;

