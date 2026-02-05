package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedDictionary`2.ValueCollection")
extern class SortedDictionary_ValueCollection<TKey, TValue> {
	var Count(default, never):Int;
	function new(dictionary:cs.system.collections.generic.SortedDictionary<TKey, TValue>):Void;
	function CopyTo(array:cs.NativeArray<TValue>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.SortedDictionary_ValueCollection_Enumerator<TKey, TValue>;
}
