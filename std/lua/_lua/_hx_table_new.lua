_G.table.new = _G.table.create or (_G.pcall(_G.require, "table.new") and _G.table.new) or function() return {} end
