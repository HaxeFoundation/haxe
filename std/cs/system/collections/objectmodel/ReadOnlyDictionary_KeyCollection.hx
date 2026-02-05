package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ReadOnlyDictionary`2.KeyCollection")
extern class ReadOnlyDictionary_KeyCollection<TKey, TValue> {
	var Count(default, never):Int;
	function CopyTo(array:cs.NativeArray<TKey>, arrayIndex:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TKey>;
}
