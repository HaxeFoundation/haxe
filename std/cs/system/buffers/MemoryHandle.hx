package cs.system.buffers;

/** Provides a memory handle for a block of memory. */
@:native("System.Buffers.MemoryHandle")
extern class MemoryHandle extends cs.system.ValueType {
	/**
	 * Returns a pointer to the memory block.
	 * @return A pointer to the memory block.
	 */
	var Pointer(default, never):cs.Pointer<Void>;
	function new(pointer:cs.Pointer<Void>, ?handle:cs.system.runtime.interopservices.GCHandle, ?pinnable:cs.system.buffers.IPinnable):Void;
	/** Frees the pinned handle and releases the  instance. */
	function Dispose():Void;
}
