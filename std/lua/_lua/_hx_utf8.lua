-- UTF-8 compatibility shim: pre-populate package.loaded['lua-utf8']
-- so that @:luaRequire('lua-utf8') finds it via the normal require mechanism.
--
-- Priority: real lua-utf8 lib > built-in utf8 module (Lua 5.3+) > do nothing
--
-- Limitations when using built-in utf8 fallback:
--   upper/lower are ASCII-only
--   gsub/gmatch/match operate on bytes, not characters

local hasLuaUtf8, luaUtf8 = pcall(require, 'lua-utf8')
if not hasLuaUtf8 then
  local hasBuiltinUtf8, builtinUtf8 = pcall(require, 'utf8')
  if hasBuiltinUtf8 then
    local compat = {}

    -- len(s, i, j, lax)
    -- Built-in utf8.len does not support the lax parameter.
    -- genlua.ml hardcodes: __lua_lib_luautf8_Utf8.len(s, nil, nil, true)
    -- When lax is true and utf8.len fails (invalid UTF-8), fall back to #s.
    compat.len = function(s, i, j, lax)
      i = i or 1
      j = j or -1
      local result, err = builtinUtf8.len(s, i, j)
      if result then
        return result
      elseif lax then
        return #s
      else
        return nil, err
      end
    end

    -- char(...) maps directly
    compat.char = builtinUtf8.char

    -- codes(s) maps directly
    compat.codes = builtinUtf8.codes

    -- byte(s, i) - return codepoint at character position i (1-based)
    -- Built-in utf8.codepoint takes byte positions, so convert first.
    -- utf8.offset(s,0) has special semantics (finds char at byte pos), so reject 0.
    compat.byte = function(s, i)
      i = i or 1
      if i == 0 then return nil end
      local ok, bytePos = pcall(builtinUtf8.offset, s, i)
      if not ok or not bytePos or bytePos > #s then return nil end
      return builtinUtf8.codepoint(s, bytePos)
    end

    -- sub(s, i, j) - substring by character positions (1-based, inclusive)
    -- Convert character positions to byte positions, then use string.sub.
    compat.sub = function(s, i, j)
      j = j or -1
      local len = builtinUtf8.len(s)
      if not len then return s:sub(i, j) end

      -- normalize negative indices
      if i < 0 then i = len + i + 1 end
      if j < 0 then j = len + j + 1 end

      -- clamp
      if i < 1 then i = 1 end
      if j > len then j = len end
      if i > j then return "" end

      local byteStart = builtinUtf8.offset(s, i)
      -- end of character j = start of character j+1 minus 1
      local byteEnd
      if j >= len then
        byteEnd = #s
      else
        byteEnd = builtinUtf8.offset(s, j + 1) - 1
      end

      return s:sub(byteStart, byteEnd)
    end

    -- find(s, pat, init, plain) - convert init and results between char/byte positions
    compat.find = function(s, pat, init, plain)
      local byteInit = nil
      if init then
        byteInit = builtinUtf8.offset(s, init)
        if not byteInit then return nil end -- init past end of string
      end
      local byteStart, byteEnd = string.find(s, pat, byteInit, plain)
      if not byteStart then return nil end
      -- convert byte positions back to character positions
      local charStart = builtinUtf8.len(s, 1, byteStart)
      local charEnd = builtinUtf8.len(s, 1, byteEnd)
      return charStart, charEnd
    end

    -- ASCII-only fallbacks
    compat.upper = string.upper
    compat.lower = string.lower

    -- Byte-level fallbacks for pattern functions
    compat.gsub = string.gsub
    compat.gmatch = string.gmatch
    compat.match = string.match

    package.loaded['lua-utf8'] = compat
  end
end
