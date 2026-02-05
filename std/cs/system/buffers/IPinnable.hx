package cs.system.buffers;

/** Provides a mechanism for pinning and unpinning objects to prevent the garbage collector from moving them. */
@:native("System.Buffers.IPinnable")
extern interface IPinnable {
	/**
	 * Pins a block of memory.
	 * @param elementIndex The offset to the element within the memory buffer to which
	 * the returned  points.
	 * @return A handle to the block of memory.
	 */
	function Pin(elementIndex:Int):cs.system.buffers.MemoryHandle;
	/** Frees a block of pinned memory. */
	function Unpin():Void;
}
