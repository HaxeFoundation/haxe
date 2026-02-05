package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary`2.ValueCollection")
extern class Dictionary_ValueCollection<TKey, TValue> {
	var Count(default, never):Int;
	function new(dictionary:cs.system.collections.generic.Dictionary<TKey, TValue>):Void;
	function CopyTo(array:cs.NativeArray<TValue>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.Dictionary_ValueCollection_Enumerator<TKey, TValue>;
}
