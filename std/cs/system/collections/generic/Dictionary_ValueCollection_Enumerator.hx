package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator")
extern class Dictionary_ValueCollection_Enumerator<TKey, TValue> extends cs.system.ValueType {
	var Current(default, never):TValue;
	function Dispose():Void;
	function MoveNext():Bool;
}
