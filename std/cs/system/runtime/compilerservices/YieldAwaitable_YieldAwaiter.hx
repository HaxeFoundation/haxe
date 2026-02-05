package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.YieldAwaitable.YieldAwaiter")
extern class YieldAwaitable_YieldAwaiter extends cs.system.ValueType {
	var IsCompleted(default, never):Bool;
	function GetResult():Void;
	function OnCompleted(continuation:cs.system.Action):Void;
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
