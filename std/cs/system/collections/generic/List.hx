package cs.system.collections.generic;

@:native("System.Collections.Generic.List")
extern class List<T> {
	var Capacity(default, default):Int;
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:native("set_Item")
	function set_Item(index0:Int, value:T):Void;
	@:overload(function():Void {})
	@:overload(function(collection:cs.system.collections.generic.IEnumerable<T>):Void {})
	function new(capacity:Int):Void;
	function Add(item:T):Void;
	function AddRange(collection:cs.system.collections.generic.IEnumerable<T>):Void;
	function AsReadOnly():cs.system.collections.objectmodel.ReadOnlyCollection<T>;
	@:overload(function(item:T):Int {})
	@:overload(function(item:T, comparer:cs.system.collections.generic.IComparer<T>):Int {})
	function BinarySearch(index:Int, count:Int, item:T, comparer:cs.system.collections.generic.IComparer<T>):Int;
	function Clear():Void;
	function Contains(item:T):Bool;
	function ConvertAll<TOutput>(converter:cs.system.Converter<T, TOutput>):cs.system.collections.generic.List<TOutput>;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	@:overload(function(array:cs.NativeArray<T>, arrayIndex:Int):Void {})
	function CopyTo(index:Int, array:cs.NativeArray<T>, arrayIndex:Int, count:Int):Void;
	function Exists(match:cs.system.Predicate<T>):Bool;
	function Find(match:cs.system.Predicate<T>):T;
	function FindAll(match:cs.system.Predicate<T>):cs.system.collections.generic.List<T>;
	@:overload(function(match:cs.system.Predicate<T>):Int {})
	@:overload(function(startIndex:Int, match:cs.system.Predicate<T>):Int {})
	function FindIndex(startIndex:Int, count:Int, match:cs.system.Predicate<T>):Int;
	function FindLast(match:cs.system.Predicate<T>):T;
	@:overload(function(match:cs.system.Predicate<T>):Int {})
	@:overload(function(startIndex:Int, match:cs.system.Predicate<T>):Int {})
	function FindLastIndex(startIndex:Int, count:Int, match:cs.system.Predicate<T>):Int;
	function ForEach(action:cs.system.Action_1<T>):Void;
	function GetEnumerator():cs.system.collections.generic.List_Enumerator<T>;
	function GetRange(index:Int, count:Int):cs.system.collections.generic.List<T>;
	@:overload(function(item:T):Int {})
	@:overload(function(item:T, index:Int):Int {})
	function IndexOf(item:T, index:Int, count:Int):Int;
	function Insert(index:Int, item:T):Void;
	function InsertRange(index:Int, collection:cs.system.collections.generic.IEnumerable<T>):Void;
	@:overload(function(item:T):Int {})
	@:overload(function(item:T, index:Int):Int {})
	function LastIndexOf(item:T, index:Int, count:Int):Int;
	function Remove(item:T):Bool;
	function RemoveAll(match:cs.system.Predicate<T>):Int;
	function RemoveAt(index:Int):Void;
	function RemoveRange(index:Int, count:Int):Void;
	@:overload(function():Void {})
	function Reverse(index:Int, count:Int):Void;
	@:overload(function():Void {})
	@:overload(function(comparer:cs.system.collections.generic.IComparer<T>):Void {})
	@:overload(function(comparison:cs.system.Comparison<T>):Void {})
	function Sort(index:Int, count:Int, comparer:cs.system.collections.generic.IComparer<T>):Void;
	function ToArray():cs.NativeArray<T>;
	function TrimExcess():Void;
	function TrueForAll(match:cs.system.Predicate<T>):Bool;
}
