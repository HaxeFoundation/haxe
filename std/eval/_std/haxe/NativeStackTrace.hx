package haxe;

import haxe.CallStack.StackItem;

private typedef NativeTrace = Array<StackItem>;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
class NativeStackTrace {
	@:ifFeature('haxe.NativeStackTrace.exceptionStack')
	static public inline function saveStack(exception:Any):Void {
	}

	static public function callStack():NativeTrace {
		return _callStack();
	}

	//implemented in the compiler
	static function _callStack():NativeTrace {
		return null;
	}

	//implemented in the compiler
	static public function exceptionStack():NativeTrace {
		return null;
	}

	static public inline function toHaxe(nativeStackTrace:NativeTrace, skip:Int = 0):Array<StackItem> {
		return skip > 0 ? nativeStackTrace.slice(skip) : nativeStackTrace;
	}
}