package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedDictionary`2.KeyCollection")
extern class SortedDictionary_KeyCollection<TKey, TValue> {
	var Count(default, never):Int;
	function new(dictionary:cs.system.collections.generic.SortedDictionary<TKey, TValue>):Void;
	function CopyTo(array:cs.NativeArray<TKey>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.SortedDictionary_KeyCollection_Enumerator<TKey, TValue>;
}
