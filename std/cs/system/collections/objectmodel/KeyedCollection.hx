package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.KeyedCollection")
extern class KeyedCollection<TKey, TItem> extends cs.system.collections.objectmodel.Collection<T1> {
	var Comparer(default, never):cs.system.collections.generic.IEqualityComparer<TKey>;
	var Dictionary(default, never):cs.system.collections.generic.IDictionary<TKey, TItem>;
	function Contains(key:TKey):Bool;
	function Remove(key:TKey):Bool;
	function TryGetValue(key:TKey, item:cs.Ref<TItem>):Bool;
}
