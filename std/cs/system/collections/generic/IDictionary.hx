package cs.system.collections.generic;

@:native("System.Collections.Generic.IDictionary")
extern interface IDictionary<TKey, TValue> extends cs.system.collections.generic.ICollection<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>> extends cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>> extends cs.system.collections.IEnumerable {
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	@:native("set_Item")
	function set_Item(index0:TKey, value:TValue):Void;
	var Keys(default, never):cs.system.collections.generic.ICollection<TKey>;
	var Values(default, never):cs.system.collections.generic.ICollection<TValue>;
	function Add(key:TKey, value:TValue):Void;
	function ContainsKey(key:TKey):Bool;
	function Remove(key:TKey):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
