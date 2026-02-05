package cs.system.io.compression;

/** Provides static methods for creating, extracting, and opening zip archives. */
@:native("System.IO.Compression.ZipFile")
extern class ZipFile {
	@:overload(function(sourceDirectoryName:String, destinationArchiveFileName:String):Void {})
	@:overload(function(sourceDirectoryName:String, destinationArchiveFileName:String, compressionLevel:cs.system.io.compression.CompressionLevel, includeBaseDirectory:Bool):Void {})
	/**
	 * Creates a zip archive that contains the files and directories from the specified
	 * directory.
	 * @param sourceDirectoryName The path to the directory to be archived, specified
	 * as a relative or absolute path. A relative path is interpreted as relative to
	 * the current working directory.
	 * @param destinationArchiveFileName The path of the archive to be created,
	 * specified as a relative or absolute path. A relative path is interpreted as
	 * relative to the current working directory.
	 */
	static function CreateFromDirectory(sourceDirectoryName:String, destinationArchiveFileName:String, compressionLevel:cs.system.io.compression.CompressionLevel, includeBaseDirectory:Bool, entryNameEncoding:cs.system.text.Encoding):Void;
	@:overload(function(sourceArchiveFileName:String, destinationDirectoryName:String):Void {})
	@:overload(function(sourceArchiveFileName:String, destinationDirectoryName:String, overwriteFiles:Bool):Void {})
	@:overload(function(sourceArchiveFileName:String, destinationDirectoryName:String, entryNameEncoding:cs.system.text.Encoding):Void {})
	/**
	 * Extracts all the files in the specified zip archive to a directory on the file
	 * system.
	 * @param sourceArchiveFileName The path to the archive that is to be extracted.
	 * @param destinationDirectoryName The path to the directory in which to place the
	 * extracted files, specified as a relative or absolute path. A relative path is
	 * interpreted as relative to the current working directory.
	 */
	static function ExtractToDirectory(sourceArchiveFileName:String, destinationDirectoryName:String, entryNameEncoding:cs.system.text.Encoding, overwriteFiles:Bool):Void;
	@:overload(function(archiveFileName:String, mode:cs.system.io.compression.ZipArchiveMode):cs.system.io.compression.ZipArchive {})
	/**
	 * Opens a zip archive at the specified path and in the specified mode.
	 * @param archiveFileName The path to the archive to open, specified as a relative
	 * or absolute path. A relative path is interpreted as relative to the current
	 * working directory.
	 * @param mode One of the enumeration values that specifies the actions which are
	 * allowed on the entries in the opened archive.
	 * @return The opened zip archive.
	 */
	static function Open(archiveFileName:String, mode:cs.system.io.compression.ZipArchiveMode, entryNameEncoding:cs.system.text.Encoding):cs.system.io.compression.ZipArchive;
	/**
	 * Opens a zip archive for reading at the specified path.
	 * @param archiveFileName The path to the archive to open, specified as a relative
	 * or absolute path. A relative path is interpreted as relative to the current
	 * working directory.
	 * @return The opened zip archive.
	 */
	static function OpenRead(archiveFileName:String):cs.system.io.compression.ZipArchive;
}
