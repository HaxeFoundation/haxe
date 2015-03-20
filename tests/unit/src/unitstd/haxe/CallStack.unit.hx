var stack = haxe.CallStack.callStack();
Std.is(stack, Array) == true;

var stack = haxe.CallStack.exceptionStack();
Std.is(stack, Array) == true;

try {
    throw false;
} catch (_:Dynamic) {
    var stack = haxe.CallStack.exceptionStack();
    Std.is(stack, Array) == true;
}
#if js
var old = @:privateAccess haxe.CallStack.lastException;
@:privateAccess haxe.CallStack.lastException = null;
var stack = haxe.CallStack.exceptionStack();
Std.is(stack, Array) == true;
stack.length == 0;
@:privateAccess haxe.CallStack.lastException = old;
#end