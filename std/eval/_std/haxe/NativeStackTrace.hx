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

	static public function callStack():Array<StackItem> {
		return _callStack();
	}

	//implemented in the compiler
	static function _callStack():Array<StackItem> {
		return null;
	}

	//implemented in the compiler
	static public function exceptionStack():Array<StackItem> {
		return null;
	}

	static public inline function toHaxe(stack:Array<StackItem>, skip:Int = 0):Array<StackItem> {
		return skip > 0 ? stack.slice(skip) : stack;
	}
}