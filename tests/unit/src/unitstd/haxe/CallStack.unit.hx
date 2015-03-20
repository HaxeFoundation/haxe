var stack = haxe.CallStack.callStack();
Std.is(stack, Array) == true;

var stack = haxe.CallStack.exceptionStack();
Std.is(stack, Array) == true;
