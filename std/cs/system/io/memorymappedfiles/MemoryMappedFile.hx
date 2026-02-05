package cs.system.io.memorymappedfiles;

/** Represents a memory-mapped file. */
@:native("System.IO.MemoryMappedFiles.MemoryMappedFile")
extern class MemoryMappedFile {
	/**
	 * Gets the file handle of a memory-mapped file.
	 * @return The handle to the memory-mapped file.
	 */
	var SafeMemoryMappedFileHandle(default, never):cs.microsoft.win32.safehandles.SafeMemoryMappedFileHandle;
	@:overload(function(path:String):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(path:String, mode:cs.system.io.FileMode):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, mapName:String):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, mapName:String, capacity:haxe.Int64):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(path:String, mode:cs.system.io.FileMode, mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	/**
	 * Creates a memory-mapped file from an existing file with the specified access
	 * mode, name, inheritability, and capacity.
	 * @param fileStream The file stream of the existing file.
	 * @param mapName A name to assign to the memory-mapped file, or  for a  that you
	 * do not intend to share across processes.
	 * @param capacity The maximum size, in bytes, to allocate to the memory-mapped
	 * file. Specify 0 to set the capacity to the size of filestream.
	 * @param access One of the enumeration values that specifies the type of access
	 * allowed to the memory-mapped file. This parameter can't be set to .
	 * @param inheritability One of the enumeration values that specifies whether a
	 * handle to the memory-mapped file can be inherited by a child process. The
	 * default is .
	 * @param leaveOpen A value that indicates whether to close the source file stream
	 * when the  is disposed.
	 * @return A memory-mapped file that has the specified characteristics.
	 */
	static function CreateFromFile(fileStream:cs.system.io.FileStream, mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess, inheritability:cs.system.io.HandleInheritability, leaveOpen:Bool):cs.system.io.memorymappedfiles.MemoryMappedFile;
	@:overload(function(mapName:String, capacity:haxe.Int64):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	/**
	 * Creates a memory-mapped file that has the specified capacity in system memory.
	 * @param mapName A name to assign to the memory-mapped file, or  for a  that you
	 * do not intend to share across processes.
	 * @param capacity The maximum size, in bytes, to allocate to the memory-mapped
	 * file.
	 * @return A memory-mapped file that has the specified name and capacity.
	 */
	static function CreateNew(mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess, options:cs.system.io.memorymappedfiles.MemoryMappedFileOptions, inheritability:cs.system.io.HandleInheritability):cs.system.io.memorymappedfiles.MemoryMappedFile;
	@:overload(function(mapName:String, capacity:haxe.Int64):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	/**
	 * Creates or opens a memory-mapped file that has the specified name and capacity
	 * in system memory.
	 * @param mapName The name of the memory-mapped file.
	 * @param capacity The maximum size, in bytes, to allocate to the memory-mapped
	 * file.
	 * @return A memory-mapped file that has the specified name and size.
	 */
	static function CreateOrOpen(mapName:String, capacity:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess, options:cs.system.io.memorymappedfiles.MemoryMappedFileOptions, inheritability:cs.system.io.HandleInheritability):cs.system.io.memorymappedfiles.MemoryMappedFile;
	@:overload(function(mapName:String):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	@:overload(function(mapName:String, desiredAccessRights:cs.system.io.memorymappedfiles.MemoryMappedFileRights):cs.system.io.memorymappedfiles.MemoryMappedFile {})
	/**
	 * Opens an existing memory-mapped file that has the specified name in system
	 * memory.
	 * @param mapName The name of the memory-mapped file.
	 * @return A memory-mapped file that has the specified name.
	 */
	static function OpenExisting(mapName:String, desiredAccessRights:cs.system.io.memorymappedfiles.MemoryMappedFileRights, inheritability:cs.system.io.HandleInheritability):cs.system.io.memorymappedfiles.MemoryMappedFile;
	@:overload(function():cs.system.io.memorymappedfiles.MemoryMappedViewAccessor {})
	@:overload(function(offset:haxe.Int64, size:haxe.Int64):cs.system.io.memorymappedfiles.MemoryMappedViewAccessor {})
	/**
	 * Creates a  that maps to a view of the memory-mapped file.
	 * @return A randomly accessible block of memory.
	 */
	function CreateViewAccessor(offset:haxe.Int64, size:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess):cs.system.io.memorymappedfiles.MemoryMappedViewAccessor;
	@:overload(function():cs.system.io.memorymappedfiles.MemoryMappedViewStream {})
	@:overload(function(offset:haxe.Int64, size:haxe.Int64):cs.system.io.memorymappedfiles.MemoryMappedViewStream {})
	/**
	 * Creates a stream that maps to a view of the memory-mapped file.
	 * @return A stream of memory.
	 */
	function CreateViewStream(offset:haxe.Int64, size:haxe.Int64, access:cs.system.io.memorymappedfiles.MemoryMappedFileAccess):cs.system.io.memorymappedfiles.MemoryMappedViewStream;
	/** Releases all resources used by the . */
	function Dispose():Void;
}
