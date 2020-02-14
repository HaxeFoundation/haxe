var stack = haxe.CallStack.callStack();
(stack is Array) == true;

var stack = haxe.CallStack.exceptionStack();
(stack is Array) == true;

function throw2() {
	throw false;
}
function throw1() {
	throw2();
}
try {
	throw1();
} catch (_:Dynamic) {
	var stack = haxe.CallStack.exceptionStack();
	(stack is Array) == true;
	stack.length > 0;
}
#if js
var old = @:privateAccess haxe.CallStack.lastException;
@:privateAccess haxe.CallStack.lastException = null;
var stack = haxe.CallStack.exceptionStack();
(stack is Array) == true;
stack.length == 0;
@:privateAccess haxe.CallStack.lastException = old;
#end