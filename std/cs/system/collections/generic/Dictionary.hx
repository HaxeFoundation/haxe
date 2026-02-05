package cs.system.collections.generic;

@:native("System.Collections.Generic.Dictionary")
extern class Dictionary<TKey, TValue> {
	var Comparer(default, never):cs.system.collections.generic.IEqualityComparer<TKey>;
	var Count(default, never):Int;
	var Keys(default, never):cs.system.collections.generic.Dictionary_KeyCollection<TKey, TValue>;
	var Values(default, never):cs.system.collections.generic.Dictionary_ValueCollection<TKey, TValue>;
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	@:native("set_Item")
	function set_Item(index0:TKey, value:TValue):Void;
	@:overload(function():Void {})
	@:overload(function(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>):Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>):Void {})
	@:overload(function(comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	@:overload(function(capacity:Int):Void {})
	@:overload(function(dictionary:cs.system.collections.generic.IDictionary<TKey, TValue>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	function new(capacity:Int, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void;
	function Add(key:TKey, value:TValue):Void;
	function Clear():Void;
	function ContainsKey(key:TKey):Bool;
	function ContainsValue(value:TValue):Bool;
	function EnsureCapacity(capacity:Int):Int;
	function GetEnumerator():cs.system.collections.generic.Dictionary_Enumerator<TKey, TValue>;
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	function OnDeserialization(sender:Dynamic):Void;
	@:overload(function(key:TKey):Bool {})
	function Remove(key:TKey, value:cs.Ref<TValue>):Bool;
	@:overload(function():Void {})
	function TrimExcess(capacity:Int):Void;
	function TryAdd(key:TKey, value:TValue):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
}
