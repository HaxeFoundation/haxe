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
	#if !lua
	stack.length > 0;
	#end
}
#if js
var old = @:privateAccess haxe.NativeStackTrace.lastError;
@:privateAccess haxe.NativeStackTrace.lastError = null;
var stack = haxe.CallStack.exceptionStack();
(stack is Array) == true;
stack.length == 0;
@:privateAccess haxe.NativeStackTrace.lastError = old;
#end