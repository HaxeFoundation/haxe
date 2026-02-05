package cs.system.io.memorymappedfiles;

/** Represents a randomly accessed view of a memory-mapped file. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedViewAccessor")
extern class MemoryMappedViewAccessor extends cs.system.io.UnmanagedMemoryAccessor {
	/**
	 * Gets the number of bytes by which the starting position of this view is offset
	 * from the beginning of the memory-mapped file.
	 * @return The number of bytes between the starting position of this view and the
	 * beginning of the memory-mapped file.
	 */
	var PointerOffset(default, never):haxe.Int64;
	/**
	 * Gets a handle to the view of a memory-mapped file.
	 * @return A wrapper for the operating system's handle to the view of the file.
	 */
	var SafeMemoryMappedViewHandle(default, never):cs.microsoft.win32.safehandles.SafeMemoryMappedViewHandle;
	/** Clears all buffers for this view and causes any buffered data to be written to the underlying file. */
	function Flush():Void;
}
