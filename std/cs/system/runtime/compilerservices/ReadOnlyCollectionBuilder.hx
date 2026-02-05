package cs.system.runtime.compilerservices;

@:native("System.Runtime.CompilerServices.ReadOnlyCollectionBuilder")
extern class ReadOnlyCollectionBuilder<T> {
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
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function IndexOf(item:T):Int;
	function Insert(index:Int, item:T):Void;
	function Remove(item:T):Bool;
	function RemoveAt(index:Int):Void;
	@:overload(function():Void {})
	function Reverse(index:Int, count:Int):Void;
	function ToArray():cs.NativeArray<T>;
	function ToReadOnlyCollection():cs.system.collections.objectmodel.ReadOnlyCollection<T>;
}
