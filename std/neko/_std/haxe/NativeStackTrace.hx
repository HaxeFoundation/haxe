package haxe;

import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	static public inline function saveStack(exception:Any):Void {
	}

	static public inline function callStack():Dynamic {
		return untyped __dollar__callstack();
	}

	static public inline function exceptionStack():Dynamic {
		return untyped __dollar__excstack();
	}

	static public function toHaxe(native:Dynamic, skip:Int = 0):Array<StackItem> {
		var a = new Array();
		var l = untyped __dollar__asize(native);
		var i = 0;
		while (i < l) {
			if(skip > i++) {
				continue;
			}
			var x = native[i];
			if (x == null)
				a.unshift(CFunction);
			else if (untyped __dollar__typeof(x) == __dollar__tstring)
				a.unshift(Module(new String(x)));
			else
				a.unshift(FilePos(null, new String(untyped x[0]), untyped x[1]));
		}
		return a;
	}
}