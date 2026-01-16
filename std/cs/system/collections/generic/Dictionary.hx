package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary")
extern class Dictionary<K, V> {
	function new():Void;

	var Count(default, never):Int;
	var Keys(default, never):KeyCollection<K, V>;
	var Values(default, never):ValueCollection<K, V>;

	function Add(key:K, value:V):Void;
	function Clear():Void;
	function ContainsKey(key:K):Bool;
	function ContainsValue(value:V):Bool;
	function Remove(key:K):Bool;
	function TryGetValue(key:K, value:V):Bool;

	@:native("get_Item") function get_Item(key:K):V;
	@:native("set_Item") function set_Item(key:K, value:V):Void;

	function GetEnumerator():Enumerator<K, V>;
}

@:native("System.Collections.Generic.Dictionary.KeyCollection")
extern class KeyCollection<K, V> {
	var Count(default, never):Int;
	function GetEnumerator():KeyEnumerator<K, V>;
}

@:native("System.Collections.Generic.Dictionary.ValueCollection")
extern class ValueCollection<K, V> {
	var Count(default, never):Int;
	function GetEnumerator():ValueEnumerator<K, V>;
}

@:native("System.Collections.Generic.Dictionary.Enumerator")
extern class Enumerator<K, V> {
	var Current(default, never):KeyValuePair<K, V>;
	function MoveNext():Bool;
	function Dispose():Void;
}

@:native("System.Collections.Generic.Dictionary.KeyCollection.Enumerator")
extern class KeyEnumerator<K, V> {
	var Current(default, never):K;
	function MoveNext():Bool;
	function Dispose():Void;
}

@:native("System.Collections.Generic.Dictionary.ValueCollection.Enumerator")
extern class ValueEnumerator<K, V> {
	var Current(default, never):V;
	function MoveNext():Bool;
	function Dispose():Void;
}

@:native("System.Collections.Generic.KeyValuePair")
extern class KeyValuePair<K, V> {
	var Key(default, never):K;
	var Value(default, never):V;
}
