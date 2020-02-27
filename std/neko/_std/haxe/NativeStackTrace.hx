package haxe;

import haxe.CallStack.StackItem;

private typedef NativeTrace = {
	final skip:Int;
	final stack:Dynamic;
}

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	static public inline function saveStack(exception:Any):Void {
	}

	static public inline function callStack():NativeTrace {
		return { skip:1, stack:untyped __dollar__callstack() };
	}

	static public function exceptionStack():NativeTrace {
		return { skip:0, stack:untyped __dollar__excstack() };
	}

	static public function toHaxe(native:NativeTrace, skip:Int = 0):Array<StackItem> {
		skip += native.skip;
		trace(skip);
		var a = new Array();
		var l = untyped __dollar__asize(native.stack);
		var i = 0;
		while (i < l) {
			var x = native.stack[i++];
			if (x == null)
				a.unshift(CFunction);
			else if (untyped __dollar__typeof(x) == __dollar__tstring)
				a.unshift(Module(new String(x)));
			else
				a.unshift(FilePos(null, new String(untyped x[0]), untyped x[1]));
		}
		return skip > 0 ? a.slice(skip) : a;
	}
}