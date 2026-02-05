package cs.system.buffers;

@:native("System.Buffers.IBufferWriter")
extern interface IBufferWriter<T> {
	function Advance(count:Int):Void;
	function GetMemory(?sizeHint:Int):cs.system.Memory<T>;
	function GetSpan(?sizeHint:Int):cs.system.Span<T>;
}
