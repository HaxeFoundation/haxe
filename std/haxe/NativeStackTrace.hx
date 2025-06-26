package haxe;

import haxe.CallStack.StackItem;

/**
	Do not use manually.
**/
@:dox(hide)
@:noCompletion
extern class NativeStackTrace {
	static public function saveStack(exception:Any):Void;
	static public function callStack():Any;
	static public function exceptionStack():Any;
	static public function toHaxe(nativeStackTrace:Any, skip:Int = 0):Array<StackItem>;
}