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
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
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
		var a = new Array();
		var l = untyped __dollar__asize(native.stack);
		var i = 0;
		while (i < l) {
			var x = native.stack[l - i - 1];
			//skip all CFunctions until we skip required amount of hx entries
			if(x == null && skip > i) {
				skip++;
			}
			if(skip > i++) {
				continue;
			}
			if (x == null)
				a.push(CFunction);
			else if (untyped __dollar__typeof(x) == __dollar__tstring)
				a.push(Module(new String(x)));
			else
				a.push(FilePos(null, new String(untyped x[0]), untyped x[1]));
		}
		return a;
	}
}