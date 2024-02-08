package haxe;

import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
	}

	@:noDebug //Do not mess up the exception stack
	static public function callStack():Array<String> {
		return untyped __global__.__hxcpp_get_call_stack(true);
	}

	@:noDebug //Do not mess up the exception stack/
	static public function exceptionStack():Array<String> {
		return untyped __global__.__hxcpp_get_exception_stack();
	}

	static public function toHaxe(native:Array<String>, skip:Int = 0):Array<StackItem> {
		var stack:Array<String> = native;
		var m = new Array<StackItem>();
		for (i in 0...stack.length) {
			if(skip > i) {
				continue;
			}
			var words = stack[i].split("::");
			if (words.length == 0)
				m.push(CFunction)
			else if (words.length == 2)
				m.push(Method(words[0], words[1]));
			else if (words.length == 4)
				m.push(FilePos(Method(words[0], words[1]), words[2], Std.parseInt(words[3])));
		}
		return m;
	}
}