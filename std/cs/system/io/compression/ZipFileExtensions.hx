package cs.system.io.compression;

/** Provides extension methods for the  and  classes. */
@:native("System.IO.Compression.ZipFileExtensions")
extern class ZipFileExtensions {
	@:overload(function(destination:cs.system.io.compression.ZipArchive, sourceFileName:String, entryName:String):cs.system.io.compression.ZipArchiveEntry {})
	/**
	 * Archives a file by compressing it and adding it to the zip archive.
	 * @param destination The zip archive to add the file to.
	 * @param sourceFileName The path to the file to be archived. You can specify
	 * either a relative or an absolute path. A relative path is interpreted as
	 * relative to the current working directory.
	 * @param entryName The name of the entry to create in the zip archive.
	 * @return A wrapper for the new entry in the zip archive.
	 */
	static function CreateEntryFromFile(destination:cs.system.io.compression.ZipArchive, sourceFileName:String, entryName:String, compressionLevel:cs.system.io.compression.CompressionLevel):cs.system.io.compression.ZipArchiveEntry;
	@:overload(function(source:cs.system.io.compression.ZipArchive, destinationDirectoryName:String):Void {})
	/**
	 * Extracts all the files in the zip archive to a directory on the file system.
	 * @param source The zip archive to extract files from.
	 * @param destinationDirectoryName The path to the directory to place the extracted
	 * files in. You can specify either a relative or an absolute path. A relative path
	 * is interpreted as relative to the current working directory.
	 */
	static function ExtractToDirectory(source:cs.system.io.compression.ZipArchive, destinationDirectoryName:String, overwriteFiles:Bool):Void;
	@:overload(function(source:cs.system.io.compression.ZipArchiveEntry, destinationFileName:String):Void {})
	/**
	 * Extracts an entry in the zip archive to a file.
	 * @param source The zip archive entry to extract a file from.
	 * @param destinationFileName The path of the file to create from the contents of
	 * the entry. You can  specify either a relative or an absolute path. A relative
	 * path is interpreted as relative to the current working directory.
	 */
	static function ExtractToFile(source:cs.system.io.compression.ZipArchiveEntry, destinationFileName:String, overwrite:Bool):Void;
}
