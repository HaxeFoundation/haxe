package cs.system.io.compression;

/** Represents a package of compressed files in the zip archive format. */
@:native("System.IO.Compression.ZipArchive")
extern class ZipArchive {
	/**
	 * Gets the collection of entries that are currently in the zip archive.
	 * @return The collection of entries that are currently in the zip archive.
	 */
	var Entries(default, never):cs.system.collections.objectmodel.ReadOnlyCollection<cs.system.io.compression.ZipArchiveEntry>;
	/**
	 * Gets a value that describes the type of action the zip archive can perform on
	 * entries.
	 * @return One of the enumeration values that describes the type of action (read,
	 * create, or update) the zip archive can perform on entries.
	 */
	var Mode(default, never):cs.system.io.compression.ZipArchiveMode;
	@:overload(function(stream:cs.system.io.Stream):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.io.compression.ZipArchiveMode):Void {})
	@:overload(function(stream:cs.system.io.Stream, mode:cs.system.io.compression.ZipArchiveMode, leaveOpen:Bool):Void {})
	function new(stream:cs.system.io.Stream, mode:cs.system.io.compression.ZipArchiveMode, leaveOpen:Bool, entryNameEncoding:cs.system.text.Encoding):Void;
	@:overload(function(entryName:String):cs.system.io.compression.ZipArchiveEntry {})
	/**
	 * Creates an empty entry that has the specified path and entry name in the zip
	 * archive.
	 * @param entryName A path, relative to the root of the archive, that specifies the
	 * name of the entry to be created.
	 * @return An empty entry in the zip archive.
	 */
	function CreateEntry(entryName:String, compressionLevel:cs.system.io.compression.CompressionLevel):cs.system.io.compression.ZipArchiveEntry;
	/** Releases the resources used by the current instance of the  class. */
	function Dispose():Void;
	/**
	 * Retrieves a wrapper for the specified entry in the zip archive.
	 * @param entryName A path, relative to the root of the archive, that identifies
	 * the entry to retrieve.
	 * @return A wrapper for the specified entry in the archive;  if the entry does not
	 * exist in the archive.
	 */
	function GetEntry(entryName:String):cs.system.io.compression.ZipArchiveEntry;
}
