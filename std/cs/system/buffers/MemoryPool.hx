package cs.system.buffers;

@:native("System.Buffers.MemoryPool")
extern class MemoryPool<T> {
	static var Shared(default, never):cs.system.buffers.MemoryPool<Dynamic>;
	var MaxBufferSize(default, never):Int;
	function Dispose():Void;
	function Rent(?minBufferSize:Int):cs.system.buffers.IMemoryOwner<T>;
}
