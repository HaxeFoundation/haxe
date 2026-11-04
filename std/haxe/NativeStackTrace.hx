package haxe;

import haxe.CallStack.StackItem;

private typedef NativeTrace = Any;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
extern class NativeStackTrace {
	static public function saveStack(exception:Any):Void;
	static public function callStack():NativeTrace;
	static public function exceptionStack():NativeTrace;
	static public function toHaxe(nativeStackTrace:NativeTrace, skip:Int = 0):Array<StackItem>;
}