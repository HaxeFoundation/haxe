package cs.system;

@:native("System.ReadOnlySpan")
extern class ReadOnlySpan<T> extends cs.system.ValueType {
	static var Empty(default, never):cs.system.ReadOnlySpan<Dynamic>;
	var IsEmpty(default, never):Bool;
	var Length(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	@:overload(function(pointer:cs.Pointer<Void>, length:Int):Void {})
	function new(array:cs.NativeArray<T>, start:Int, length:Int):Void;
	static function op_Equality<T>(left:cs.system.ReadOnlySpan<T>, right:cs.system.ReadOnlySpan<T>):Bool;
	@:overload(function<T>(segment:cs.system.ArraySegment<T>):cs.system.ReadOnlySpan<T> {})
	static function op_Implicit<T>(array:cs.NativeArray<T>):cs.system.ReadOnlySpan<T>;
	static function op_Inequality<T>(left:cs.system.ReadOnlySpan<T>, right:cs.system.ReadOnlySpan<T>):Bool;
	function CopyTo(destination:cs.system.Span<T>):Void;
	function Equals(obj:Dynamic):Bool;
	function GetEnumerator():cs.system.ReadOnlySpan_Enumerator<T>;
	function GetHashCode():Int;
	function GetPinnableReference():T;
	@:overload(function(start:Int):cs.system.ReadOnlySpan<T> {})
	function Slice(start:Int, length:Int):cs.system.ReadOnlySpan<T>;
	function ToArray():cs.NativeArray<T>;
	function ToString():String;
	function TryCopyTo(destination:cs.system.Span<T>):Bool;
}
