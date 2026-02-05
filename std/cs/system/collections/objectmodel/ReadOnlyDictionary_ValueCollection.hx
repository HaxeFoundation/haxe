package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ReadOnlyDictionary`2.ValueCollection")
extern class ReadOnlyDictionary_ValueCollection<TKey, TValue> {
	var Count(default, never):Int;
	function CopyTo(array:cs.NativeArray<TValue>, arrayIndex:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TValue>;
}
