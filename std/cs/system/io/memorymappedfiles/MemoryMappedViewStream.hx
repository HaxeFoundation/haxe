package cs.system.io.memorymappedfiles;

/** Represents a view of a memory-mapped file as a sequentially accessed stream. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedViewStream")
extern class MemoryMappedViewStream extends cs.system.io.UnmanagedMemoryStream {
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
	/** Clears all buffers for this stream and causes any buffered data to be written to the underlying file. */
	function Flush():Void;
	/**
	 * Sets the length of the current stream.
	 * @param value The desired length of the current stream in bytes.
	 */
	function SetLength(value:haxe.Int64):Void;
}
