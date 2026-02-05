package cs.system.collections.concurrent;

@:native("System.Collections.Concurrent.ConcurrentDictionary")
extern class ConcurrentDictionary<TKey, TValue> {
	var Count(default, never):Int;
	var IsEmpty(default, never):Bool;
	var Keys(default, never):cs.system.collections.generic.ICollection<TKey>;
	var Values(default, never):cs.system.collections.generic.ICollection<TValue>;
	@:native("get_Item")
	function get_Item(index0:TKey):TValue;
	@:native("set_Item")
	function set_Item(index0:TKey, value:TValue):Void;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>):Void {})
	@:overload(function(comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	@:overload(function(concurrencyLevel:Int, capacity:Int):Void {})
	@:overload(function(concurrencyLevel:Int, collection:cs.system.collections.generic.IEnumerable<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void {})
	function new(concurrencyLevel:Int, capacity:Int, comparer:cs.system.collections.generic.IEqualityComparer<TKey>):Void;
	@:overload(function(key:TKey, addValueFactory:cs.system.Func_2<TKey, TValue>, updateValueFactory:cs.system.Func_3<TKey, TValue, TValue>):TValue {})
	@:overload(function(key:TKey, addValue:TValue, updateValueFactory:cs.system.Func_3<TKey, TValue, TValue>):TValue {})
	function AddOrUpdate<TArg>(key:TKey, addValueFactory:cs.system.Func_3<TKey, TArg, TValue>, updateValueFactory:cs.system.Func_4<TKey, TValue, TArg, TValue>, factoryArgument:TArg):TValue;
	function Clear():Void;
	function ContainsKey(key:TKey):Bool;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>;
	@:overload(function(key:TKey, valueFactory:cs.system.Func_2<TKey, TValue>):TValue {})
	@:overload(function(key:TKey, value:TValue):TValue {})
	function GetOrAdd<TArg>(key:TKey, valueFactory:cs.system.Func_3<TKey, TArg, TValue>, factoryArgument:TArg):TValue;
	function ToArray():cs.NativeArray<cs.system.collections.generic.KeyValuePair_2<TKey, TValue>>;
	function TryAdd(key:TKey, value:TValue):Bool;
	function TryGetValue(key:TKey, value:cs.Ref<TValue>):Bool;
	function TryRemove(key:TKey, value:cs.Ref<TValue>):Bool;
	function TryUpdate(key:TKey, newValue:TValue, comparisonValue:TValue):Bool;
}
