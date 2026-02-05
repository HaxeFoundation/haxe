package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConfiguredValueTaskAwaitable`1.ConfiguredValueTaskAwaiter")
extern class ConfiguredValueTaskAwaitable_1_ConfiguredValueTaskAwaiter<TResult> extends cs.system.ValueType {
	var IsCompleted(default, never):Bool;
	function GetResult():TResult;
	function OnCompleted(continuation:cs.system.Action):Void;
	function UnsafeOnCompleted(continuation:cs.system.Action):Void;
}
