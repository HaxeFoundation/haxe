package cs.system.io.enumeration;

/** Provides a lower level view of  to help process and filter find results. */
@:native("System.IO.Enumeration.FileSystemEntry")
extern class FileSystemEntry extends cs.system.ValueType {
	/**
	 * Gets the attributes for this entry.
	 * @return The attributes for this entry.
	 */
	var Attributes(default, never):cs.system.io.FileAttributes;
	/**
	 * Gets the creation time for the entry or the oldest available time stamp if the
	 * operating system does not support creation time stamps.
	 * @return The creation time for the entry.
	 */
	var CreationTimeUtc(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets the full path of the directory this entry resides in.
	 * @return The full path of this entry's directory.
	 */
	var Directory(default, never):cs.system.ReadOnlySpan<cs.Char16>;
	/**
	 * Gets the file name for this entry.
	 * @return This entry's file name.
	 */
	var FileName(default, never):cs.system.ReadOnlySpan<cs.Char16>;
	/**
	 * Gets a value that indicates whether this entry is a directory.
	 * @return if the entry is a directory; otherwise, .
	 */
	var IsDirectory(default, never):Bool;
	/**
	 * Gets a value that indicates whether the file has the hidden attribute.
	 * @return if the file has the hidden attribute; otherwise, .
	 */
	var IsHidden(default, never):Bool;
	/**
	 * Gets a datetime offset that represents the last access time in UTC.
	 * @return The last access time in UTC.
	 */
	var LastAccessTimeUtc(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets a datetime offset that represents the last write time in UTC.
	 * @return The last write time in UTC.
	 */
	var LastWriteTimeUtc(default, never):cs.system.DateTimeOffset;
	/**
	 * Gets the length of the file, in bytes.
	 * @return The file length in bytes.
	 */
	var Length(default, never):haxe.Int64;
	/**
	 * Gets the root directory for the enumeration as specified in the constructor.
	 * @return The original root directory.
	 */
	var OriginalRootDirectory(default, never):cs.system.ReadOnlySpan<cs.Char16>;
	/**
	 * Gets the full path of the root directory used for the enumeration.
	 * @return The root directory.
	 */
	var RootDirectory(default, never):cs.system.ReadOnlySpan<cs.Char16>;
	/**
	 * Converts the value of this instance to a .
	 * @return The value of this instance as a .
	 */
	function ToFileSystemInfo():cs.system.io.FileSystemInfo;
	/**
	 * Returns the full path of the find result.
	 * @return A string representing the full path.
	 */
	function ToFullPath():String;
	/**
	 * Returns the full path for the find results, based on the initially provided
	 * path.
	 * @return A string representing the full path.
	 */
	function ToSpecifiedFullPath():String;
}
