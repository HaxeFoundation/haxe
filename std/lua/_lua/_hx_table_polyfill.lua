
table.pack=table.pack or pack or function(...) return { n=select('#',...),...} end
table.unpack=table.unpack or unpack or function(t, i) i = i or 1 if t[i] ~= nil then return t[i], table.unpack(t, i + 1) end end
table.maxn=table.maxn or function(t) local maxn=0 for i in pairs(t) do maxn=type(i)=='number'and i>maxn and i or maxn end return maxn end
