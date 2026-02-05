package cs.system.io.compression;

/** Represents a compressed file within a zip archive. */
@:native("System.IO.Compression.ZipArchiveEntry")
extern class ZipArchiveEntry {
	/**
	 * Gets the zip archive that the entry belongs to.
	 * @return The zip archive that the entry belongs to, or  if the entry has been
	 * deleted.
	 */
	var Archive(default, never):cs.system.io.compression.ZipArchive;
	/**
	 * Gets the compressed size of the entry in the zip archive.
	 * @return The compressed size of the entry in the zip archive.
	 */
	var CompressedLength(default, never):haxe.Int64;
	var Crc32(default, never):cs.UInt;
	/**
	 * OS and application specific file attributes.
	 * @return The external attributes written by the application when this entry was
	 * written. It is both host OS and application dependent.
	 */
	var ExternalAttributes(default, default):Int;
	/**
	 * Gets the relative path of the entry in the zip archive.
	 * @return The relative path of the entry in the zip archive.
	 */
	var FullName(default, never):String;
	/**
	 * Gets or sets the last time the entry in the zip archive was changed.
	 * @return The last time the entry in the zip archive was changed.
	 */
	var LastWriteTime(default, default):cs.system.DateTimeOffset;
	/**
	 * Gets the uncompressed size of the entry in the zip archive.
	 * @return The uncompressed size of the entry in the zip archive.
	 */
	var Length(default, never):haxe.Int64;
	/**
	 * Gets the file name of the entry in the zip archive.
	 * @return The file name of the entry in the zip archive.
	 */
	var Name(default, never):String;
	/** Deletes the entry from the zip archive. */
	function Delete():Void;
	/**
	 * Opens the entry from the zip archive.
	 * @return The stream that represents the contents of the entry.
	 */
	function Open():cs.system.io.Stream;
	/**
	 * Retrieves the relative path of the entry in the zip archive.
	 * @return The relative path of the entry, which is the value stored in the 
	 * property.
	 */
	function ToString():String;
}
