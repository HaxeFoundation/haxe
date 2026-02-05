package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConditionalWeakTable")
extern class ConditionalWeakTable<TKey, TValue> {
	function new():Void;
	function Add(key:TKey, value:TValue):Void;
	function AddOrUpdate(key:TKey, value:TValue):Void;
	function Clear():Void;
	function GetOrCreateValue(key:TKey):TValue;
	function GetValue(key:TKey, createValueCallback:cs.system.runtime.compilerservices.ConditionalWeakTable_CreateValueCallback<TKey, TValue>):TValue;
	function Remove(key:TKey):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
