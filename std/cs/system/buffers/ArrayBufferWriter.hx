package cs.system.buffers;

@:native("System.Buffers.ArrayBufferWriter")
extern class ArrayBufferWriter<T> {
	var Capacity(default, never):Int;
	var FreeCapacity(default, never):Int;
	var WrittenCount(default, never):Int;
	var WrittenMemory(default, never):cs.system.ReadOnlyMemory<T>;
	var WrittenSpan(default, never):cs.system.ReadOnlySpan<T>;
	@:overload(function():Void {})
	function new(initialCapacity:Int):Void;
	function Advance(count:Int):Void;
	function Clear():Void;
	function GetMemory(?sizeHint:Int):cs.system.Memory<T>;
	function GetSpan(?sizeHint:Int):cs.system.Span<T>;
}
