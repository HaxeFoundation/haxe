package cs.system.buffers;

@:native("System.Buffers.MemoryManager")
extern class MemoryManager<T> {
	var Memory(default, never):cs.system.Memory<T>;
	function GetSpan():cs.system.Span<T>;
	function Pin(?elementIndex:Int):cs.system.buffers.MemoryHandle;
	function Unpin():Void;
}
