package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ConditionalWeakTable`2.CreateValueCallback")
extern class ConditionalWeakTable_CreateValueCallback<TKey, TValue> extends cs.system.MulticastDelegate {
	function new(func:(key:TKey)->TValue):Void;
	function Invoke(key:TKey):TValue;
}
