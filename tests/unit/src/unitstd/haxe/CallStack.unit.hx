var stack = haxe.CallStack.callStack();
(stack is Array) == true;

var stack = haxe.CallStack.exceptionStack();
(stack is Array) == true;

try {
    throw false;
} catch (_:Dynamic) {
    var stack = haxe.CallStack.exceptionStack();
    (stack is Array) == true;
}
#if js
var old = @:privateAccess haxe.CallStack.lastException;
@:privateAccess haxe.CallStack.lastException = null;
var stack = haxe.CallStack.exceptionStack();
(stack is Array) == true;
stack.length == 0;
@:privateAccess haxe.CallStack.lastException = old;
#end