-- error in loaded script should be caught here, instead of exiting everything
local success, err = pcall(require, arg[1])

local exception_message = string.match(tostring(err), '^[^\n]+')
local has_expected_message = string.match(exception_message, arg[2]) ~= nil
local has_stack_trace = string.match(tostring(err), 'stack traceback') ~= nil

print('Success: '..tostring(success))
print('Has expected exception message: '..tostring(has_expected_message))
print('Has call stack: '..tostring(has_stack_trace))
