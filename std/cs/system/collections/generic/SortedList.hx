package cs.system.collections.generic;

@:native("System.Collections.Generic.SortedList")
extern class SortedList<TKey, TValue> {
	var Capacity(default, default):Int;
	var Comparer(default, never):cs.system.collections.generic.IComparer<TKey>;
	var Count(default, never):Int;
	var Keys(default, never):cs.system.collections.generic.IList<TKey>;
	var Values(default, never):cs.system.collections.generic.IList<TValue>;
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	@:native("set_Item")
	function set_Item(index0:TKey, value:TValue):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.generic.IComparer<TKey>):Void {})
	@:overload(function(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>, comparer:cs.system.collections.generic.IComparer<TKey>):Void {})
	function new(capacity:Int, comparer:cs.system.collections.generic.IComparer<TKey>):Void;
	function Add(key:TKey, value:TValue):Void;
	function Clear():Void;
	function ContainsKey(key:TKey):Bool;
	function ContainsValue(value:TValue):Bool;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>;
	function IndexOfKey(key:TKey):Int;
	function IndexOfValue(value:TValue):Int;
	function Remove(key:TKey):Bool;
	function RemoveAt(index:Int):Void;
	function TrimExcess():Void;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
