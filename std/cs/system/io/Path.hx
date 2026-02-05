package cs.system.io;

/** Performs operations on  instances that contain file or directory path information. These operations are performed in a cross-platform manner. */
@:native("System.IO.Path")
extern class Path {
	/** Provides a platform-specific alternate character used to separate directory levels in a path string that reflects a hierarchical file system organization. */
	static var AltDirectorySeparatorChar(default, never):cs.Char16;
	/** Provides a platform-specific character used to separate directory levels in a path string that reflects a hierarchical file system organization. */
	static var DirectorySeparatorChar(default, never):cs.Char16;
	/** Provides a platform-specific array of characters that cannot be specified in path string arguments passed to members of the  class. */
	static var InvalidPathChars(default, never):cs.NativeArray<cs.Char16>;
	/** A platform-specific separator character used to separate path strings in environment variables. */
	static var PathSeparator(default, never):cs.Char16;
	/** Provides a platform-specific volume separator character. */
	static var VolumeSeparatorChar(default, never):cs.Char16;
	/**
	 * Changes the extension of a path string.
	 * @param path The path information to modify. The path cannot contain any of the
	 * characters defined in .
	 * @param extension The new extension (with or without a leading period). Specify 
	 * to remove an existing extension from .
	 * @return The modified path information. On Windows-based desktop platforms, if 
	 * is  or an empty string (""), the path information is returned unmodified. If  is
	 * , the returned string contains the specified path with its extension removed. If
	 * has no extension, and  is not , the returned path string contains  appended to
	 * the end of .
	 */
	static function ChangeExtension(path:String, extension:String):String;
	@:overload(function(paths:cs.NativeArray<String>):String {})
	@:overload(function(path1:String, path2:String):String {})
	@:overload(function(path1:String, path2:String, path3:String):String {})
	/**
	 * Combines two strings into a path.
	 * @param path1 The first path to combine.
	 * @param path2 The second path to combine.
	 * @return The combined paths. If one of the specified paths is a zero-length
	 * string, this method returns the other path. If  contains an absolute path, this
	 * method returns .
	 */
	static function Combine(path1:String, path2:String, path3:String, path4:String):String;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Returns the directory information for the specified path represented by a
	 * character span.
	 * @param path The path to retrieve the directory information from.
	 * @return Directory information for , or an empty span if  is , an empty span, or
	 * a root (such as , C:, or \server\share).
	 */
	static function GetDirectoryName(path:String):String;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Returns the extension of a file path that is represented by a read-only
	 * character span.
	 * @param path The file path from which to get the extension.
	 * @return The extension of the specified path (including the period, "."), or  if 
	 * does not have extension information.
	 */
	static function GetExtension(path:String):String;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Returns the file name and extension of a file path that is represented by a
	 * read-only character span.
	 * @param path A read-only span that contains the path from which to obtain the
	 * file name and extension.
	 * @return The characters after the last directory separator character in .
	 */
	static function GetFileName(path:String):String;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Returns the file name without the extension of a file path that is represented
	 * by a read-only character span.
	 * @param path A read-only span that contains the path from which to obtain the
	 * file name without the extension.
	 * @return The characters in the read-only span returned by , minus the last period
	 * (.) and all characters following it.
	 */
	static function GetFileNameWithoutExtension(path:String):String;
	@:overload(function(path:String):String {})
	/**
	 * Returns the absolute path for the specified path string.
	 * @param path The file or directory for which to obtain absolute path information.
	 * @return The fully qualified location of , such as "C:\MyFile.txt".
	 */
	static function GetFullPath(path:String, basePath:String):String;
	/**
	 * Gets an array containing the characters that are not allowed in file names.
	 * @return An array containing the characters that are not allowed in file names.
	 */
	static function GetInvalidFileNameChars():cs.NativeArray<cs.Char16>;
	/**
	 * Gets an array containing the characters that are not allowed in path names.
	 * @return An array containing the characters that are not allowed in path names.
	 */
	static function GetInvalidPathChars():cs.NativeArray<cs.Char16>;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):cs.system.ReadOnlySpan<cs.Char16> {})
	/**
	 * Gets the root directory information from the path contained in the specified
	 * character span.
	 * @param path The path from which to obtain root directory information.
	 * @return A character span containing the root directory of path.
	 */
	static function GetPathRoot(path:String):String;
	/**
	 * Returns a random folder name or file name.
	 * @return A random folder name or file name.
	 */
	static function GetRandomFileName():String;
	/**
	 * Returns a relative path from one path to another.
	 * @param relativeTo The source path the result should be relative to. This path is
	 * always considered to be a directory.
	 * @param path The destination path.
	 * @return The relative path, or  if the paths don't share the same root.
	 */
	static function GetRelativePath(relativeTo:String, path:String):String;
	/**
	 * Creates a uniquely named, zero-byte temporary file on disk and returns the full
	 * path of that file.
	 * @return The full path of the temporary file.
	 */
	static function GetTempFileName():String;
	/**
	 * Returns the path of the current user's temporary folder.
	 * @return The path to the temporary folder, ending with a backslash.
	 */
	static function GetTempPath():String;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):Bool {})
	/**
	 * Determines whether the path represented by the specified character span includes
	 * a file name extension.
	 * @param path The path to search for an extension.
	 * @return if the characters that follow the last directory separator character or
	 * volume separator in the path include a period (".") followed by one or more
	 * characters; otherwise, .
	 */
	static function HasExtension(path:String):Bool;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):Bool {})
	/**
	 * Returns a value that indicates whether the file path represented by the
	 * specified character span is fixed to a specific drive or UNC path.
	 * @param path A file path.
	 * @return if the path is fixed to a specific drive or UNC path;  if the path is
	 * relative to the current drive or working directory.
	 */
	static function IsPathFullyQualified(path:String):Bool;
	@:overload(function(path:cs.system.ReadOnlySpan<cs.Char16>):Bool {})
	/**
	 * Returns a value that indicates whether the specified character span that
	 * represents a file path contains a root.
	 * @param path The path to test.
	 * @return if  contains a root; otherwise, .
	 */
	static function IsPathRooted(path:String):Bool;
	@:overload(function(path1:cs.system.ReadOnlySpan<cs.Char16>, path2:cs.system.ReadOnlySpan<cs.Char16>):String {})
	/**
	 * Concatenates two path components into a single path.
	 * @param path1 A character span that contains the first path to join.
	 * @param path2 A character span that contains the second path to join.
	 * @return The combined paths.
	 */
	static function Join(path1:cs.system.ReadOnlySpan<cs.Char16>, path2:cs.system.ReadOnlySpan<cs.Char16>, path3:cs.system.ReadOnlySpan<cs.Char16>):String;
	@:overload(function(path1:cs.system.ReadOnlySpan<cs.Char16>, path2:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>):Bool {})
	/**
	 * Attempts to concatenate two path components to a single preallocated character
	 * span, and returns a value that indicates whether the operation succeeded.
	 * @param path1 A character span that contains the first path to join.
	 * @param path2 A character span that contains the second path to join.
	 * @param path3 A character span that contains the third path to join.
	 * @param destination A character span to hold the concatenated path.
	 * @param charsWritten When the method returns, a value that indicates the number
	 * of characters written to the .
	 * @return if the concatenation operation is successful; otherwise, .
	 */
	static function TryJoin(path1:cs.system.ReadOnlySpan<cs.Char16>, path2:cs.system.ReadOnlySpan<cs.Char16>, path3:cs.system.ReadOnlySpan<cs.Char16>, destination:cs.system.Span<cs.Char16>, charsWritten:cs.Ref<Int>):Bool;
}
