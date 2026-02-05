package cs.system.collections.generic;

@:native("System.Collections.Generic.IReadOnlyDictionary")
extern interface IReadOnlyDictionary<TKey, TValue> extends cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>> extends cs.system.collections.IEnumerable extends cs.system.collections.generic.IReadOnlyCollection<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>> {
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	var Keys(default, never):cs.system.collections.generic.IEnumerable<TKey>;
	var Values(default, never):cs.system.collections.generic.IEnumerable<TValue>;
	function ContainsKey(key:TKey):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
