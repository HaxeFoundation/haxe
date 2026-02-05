package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ReadOnlyDictionary")
extern class ReadOnlyDictionary<TKey, TValue> {
	var Count(default, never):Int;
	var Dictionary(default, never):cs.system.collections.generic.IDictionary<TKey, TValue>;
	var Keys(default, never):cs.system.collections.objectmodel.ReadOnlyDictionary_KeyCollection<TKey, TValue>;
	var Values(default, never):cs.system.collections.objectmodel.ReadOnlyDictionary_ValueCollection<TKey, TValue>;
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	function new(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>):Void;
	function ContainsKey(key:TKey):Bool;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
