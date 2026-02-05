package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedDictionary`2.KeyCollection.Enumerator")
extern class SortedDictionary_KeyCollection_Enumerator<TKey, TValue> extends cs.system.ValueType {
	var Current(default, never):TKey;
	function Dispose():Void;
	function MoveNext():Bool;
}
