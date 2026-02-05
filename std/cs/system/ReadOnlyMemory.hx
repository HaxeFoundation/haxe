package cs.system;

@:native("System.ReadOnlyMemory")
extern class ReadOnlyMemory<T> extends cs.system.ValueType {
	static var Empty(default, never):cs.system.ReadOnlyMemory<Dynamic>;
	var IsEmpty(default, never):Bool;
	var Length(default, never):Int;
	var Span(default, never):cs.system.ReadOnlySpan<T>;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	function new(array:cs.NativeArray<T>, start:Int, length:Int):Void;
	@:overload(function<T>(segment:cs.system.ArraySegment<T>):cs.system.ReadOnlyMemory<T> {})
	static function op_Implicit<T>(array:cs.NativeArray<T>):cs.system.ReadOnlyMemory<T>;
	function CopyTo(destination:cs.system.Memory<T>):Void;
	@:overload(function(obj:Dynamic):Bool {})
	function Equals(other:cs.system.ReadOnlyMemory<T>):Bool;
	function GetHashCode():Int;
	function Pin():cs.system.buffers.MemoryHandle;
	@:overload(function(start:Int):cs.system.ReadOnlyMemory<T> {})
	function Slice(start:Int, length:Int):cs.system.ReadOnlyMemory<T>;
	function ToArray():cs.NativeArray<T>;
	function ToString():String;
	function TryCopyTo(destination:cs.system.Memory<T>):Bool;
}
