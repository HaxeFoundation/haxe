-- error in loaded script should be caught here, instead of exiting everything
local success, err = pcall(require, arg[1])

local first_line = string.match(tostring(err), '^[^\n]+')
-- replace line number with _
local exception_message = string.gsub(first_line, ':%d+:', ':_:')
local has_stack_trace = string.match(tostring(err), 'stack traceback') ~= nil

print('Success: '..tostring(success))
print('Exception message: '..exception_message)
print('Has call stack: '..tostring(has_stack_trace))
