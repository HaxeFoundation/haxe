package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConfiguredTaskAwaitable`1.ConfiguredTaskAwaiter")
extern class ConfiguredTaskAwaitable_1_ConfiguredTaskAwaiter<TResult> extends cs.system.ValueType {
	var IsCompleted(default, never):Bool;
	function GetResult():TResult;
	function OnCompleted(continuation:cs.system.Action):Void;
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
