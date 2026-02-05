package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary`2.Enumerator")
extern class Dictionary_Enumerator<TKey, TValue> extends cs.system.ValueType {
	var Current(default, never):cs.system.collections.generic.KeyValuePair_2<TKey, TValue>;
	function Dispose():Void;
	function MoveNext():Bool;
}
