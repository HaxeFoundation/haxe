package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedDictionary")
extern class SortedDictionary<TKey, TValue> {
	var Comparer(default, never):cs.system.collections.generic.IComparer<TKey>;
	var Count(default, never):Int;
	var Keys(default, never):cs.system.collections.generic.SortedDictionary_KeyCollection<TKey, TValue>;
	var Values(default, never):cs.system.collections.generic.SortedDictionary_ValueCollection<TKey, TValue>;
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	@:native("set_Item")
	function set_Item(index0:TKey, value:TValue):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.generic.IComparer<TKey>):Void {})
	@:overload(function(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>):Void {})
	function new(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>, comparer:cs.system.collections.generic.IComparer<TKey>):Void;
	function Add(key:TKey, value:TValue):Void;
	function Clear():Void;
	function ContainsKey(key:TKey):Bool;
	function ContainsValue(value:TValue):Bool;
	function CopyTo(array:cs.NativeArray<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.SortedDictionary_Enumerator<TKey, TValue>;
	function Remove(key:TKey):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
