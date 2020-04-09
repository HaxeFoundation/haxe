package haxe;

import haxe.CallStack.StackItem;

private typedef NativeTrace = Array<python.Tuple.Tuple4<String, Int, String, String>>;

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
		var infos = python.lib.Traceback.extract_stack();
		infos.pop();
		infos.reverse();
		return infos;
	}

	static public function exceptionStack():NativeTrace {
		var exc = python.lib.Sys.exc_info();
		if (exc._3 != null) {
			var infos = python.lib.Traceback.extract_tb(exc._3);
			infos.reverse();
			return infos;
		} else {
			return [];
		}
	}

	static public function toHaxe(native:NativeTrace, skip:Int = 0):Array<StackItem> {
		var stack = [];
		for(i in 0...native.length) {
			if(skip > i) {
				continue;
			}
			var elem = native[i];
			stack.push(FilePos(Method(null, elem._3), elem._1, elem._2));
		}
		return stack;
	}
}