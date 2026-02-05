package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary`2.KeyCollection")
extern class Dictionary_KeyCollection<TKey, TValue> {
	var Count(default, never):Int;
	function new(dictionary:cs.system.collections.generic.Dictionary<TKey, TValue>):Void;
	function CopyTo(array:cs.NativeArray<TKey>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.Dictionary_KeyCollection_Enumerator<TKey, TValue>;
}
