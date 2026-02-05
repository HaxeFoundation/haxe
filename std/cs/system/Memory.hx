package cs.system;

@:native("System.Memory")
extern class Memory<T> extends cs.system.ValueType {
	static var Empty(default, never):cs.system.Memory<Dynamic>;
	var IsEmpty(default, never):Bool;
	var Length(default, never):Int;
	var Span(default, never):cs.system.Span<T>;
	@:overload(function(array:cs.NativeArray<T>):Void {})
	function new(array:cs.NativeArray<T>, start:Int, length:Int):Void;
	@:overload(function<T>(segment:cs.system.ArraySegment<T>):cs.system.Memory<T> {})
	@:overload(function<T>(memory:cs.system.Memory<T>):cs.system.ReadOnlyMemory<T> {})
	static function op_Implicit<T>(array:cs.NativeArray<T>):cs.system.Memory<T>;
	function CopyTo(destination:cs.system.Memory<T>):Void;
	@:overload(function(other:cs.system.Memory<T>):Bool {})
	function Equals(obj:Dynamic):Bool;
	function GetHashCode():Int;
	function Pin():cs.system.buffers.MemoryHandle;
	@:overload(function(start:Int):cs.system.Memory<T> {})
	function Slice(start:Int, length:Int):cs.system.Memory<T>;
	function ToArray():cs.NativeArray<T>;
	function ToString():String;
	function TryCopyTo(destination:cs.system.Memory<T>):Bool;
}
