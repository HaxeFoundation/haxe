package cs.system.collections.generic;

@:native("System.Collections.Generic.KeyValuePair`2")
extern class KeyValuePair_2<TKey, TValue> extends cs.system.ValueType {
	var Key(default, never):TKey;
	var Value(default, never):TValue;
	function new(key:TKey, value:TValue):Void;
	function Deconstruct(key:cs.Ref<TKey>, value:cs.Ref<TValue>):Void;
	function ToString():String;
}
