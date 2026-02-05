package cs.system;

@:native("System.ArraySegment")
extern class ArraySegment<T> extends cs.system.ValueType {
	static var Empty(default, never):cs.system.ArraySegment<Dynamic>;
	var Array(default, never):cs.NativeArray<T>;
	var Count(default, never):Int;
	var Offset(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:native("set_Item")
	function set_Item(index0:Int, value:T):Void;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	function new(array:cs.NativeArray<T>, offset:Int, count:Int):Void;
	static function op_Equality<T>(a:cs.system.ArraySegment<T>, b:cs.system.ArraySegment<T>):Bool;
	static function op_Implicit<T>(array:cs.NativeArray<T>):cs.system.ArraySegment<T>;
	static function op_Inequality<T>(a:cs.system.ArraySegment<T>, b:cs.system.ArraySegment<T>):Bool;
	@:overload(function(destination:cs.system.ArraySegment<T>):Void {})
	@:overload(function(destination:cs.NativeArray<T>):Void {})
	function CopyTo(destination:cs.NativeArray<T>, destinationIndex:Int):Void;
	@:overload(function(obj:cs.system.ArraySegment<T>):Bool {})
	function Equals(obj:Dynamic):Bool;
	function GetEnumerator():cs.system.ArraySegment_Enumerator<T>;
	function GetHashCode():Int;
	@:overload(function(index:Int):cs.system.ArraySegment<T> {})
	function Slice(index:Int, count:Int):cs.system.ArraySegment<T>;
	function ToArray():cs.NativeArray<T>;
}
