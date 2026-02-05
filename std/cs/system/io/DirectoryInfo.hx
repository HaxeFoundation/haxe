package cs.system.io;

/** Exposes instance methods for creating, moving, and enumerating through directories and subdirectories. This class cannot be inherited. */
@:native("System.IO.DirectoryInfo")
extern class DirectoryInfo extends cs.system.io.FileSystemInfo {
	/**
	 * Gets the parent directory of a specified subdirectory.
	 * @return The parent directory, or  if the path is null or if the file path
	 * denotes a root (such as "\", "C:", or * "\\server\share").
	 */
	var Parent(default, never):cs.system.io.DirectoryInfo;
	/**
	 * Gets the root portion of the directory.
	 * @return An object that represents the root of the directory.
	 */
	var Root(default, never):cs.system.io.DirectoryInfo;
	function new(path:String):Void;
	/** Creates a directory. */
	function Create():Void;
	/**
	 * Creates a subdirectory or subdirectories on the specified path. The specified
	 * path can be relative to this instance of the  class.
	 * @param path The specified path. This cannot be a different disk volume or
	 * Universal Naming Convention (UNC) name.
	 * @return The last directory specified in .
	 */
	function CreateSubdirectory(path:String):cs.system.io.DirectoryInfo;
	@:overload(function():Void {})
	/** Deletes this  if it is empty. */
	function Delete(recursive:Bool):Void;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.io.DirectoryInfo> {})
	@:overload(function(searchPattern:String):cs.system.collections.generic.IEnumerable<cs.system.io.DirectoryInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<cs.system.io.DirectoryInfo> {})
	/**
	 * Returns an enumerable collection of directory information in the current
	 * directory.
	 * @return An enumerable collection of directories in the current directory.
	 */
	function EnumerateDirectories(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<cs.system.io.DirectoryInfo>;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.io.FileInfo> {})
	@:overload(function(searchPattern:String):cs.system.collections.generic.IEnumerable<cs.system.io.FileInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<cs.system.io.FileInfo> {})
	/**
	 * Returns an enumerable collection of file information in the current directory.
	 * @return An enumerable collection of the files in the current directory.
	 */
	function EnumerateFiles(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<cs.system.io.FileInfo>;
	@:overload(function():cs.system.collections.generic.IEnumerable<cs.system.io.FileSystemInfo> {})
	@:overload(function(searchPattern:String):cs.system.collections.generic.IEnumerable<cs.system.io.FileSystemInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<cs.system.io.FileSystemInfo> {})
	/**
	 * Returns an enumerable collection of file system information in the current
	 * directory.
	 * @return An enumerable collection of file system information in the current
	 * directory.
	 */
	function EnumerateFileSystemInfos(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<cs.system.io.FileSystemInfo>;
	@:overload(function():cs.NativeArray<cs.system.io.DirectoryInfo> {})
	@:overload(function(searchPattern:String):cs.NativeArray<cs.system.io.DirectoryInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<cs.system.io.DirectoryInfo> {})
	/**
	 * Returns the subdirectories of the current directory.
	 * @return An array of  objects.
	 */
	function GetDirectories(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<cs.system.io.DirectoryInfo>;
	@:overload(function():cs.NativeArray<cs.system.io.FileInfo> {})
	@:overload(function(searchPattern:String):cs.NativeArray<cs.system.io.FileInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<cs.system.io.FileInfo> {})
	/**
	 * Returns a file list from the current directory.
	 * @return An array of type .
	 */
	function GetFiles(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<cs.system.io.FileInfo>;
	@:overload(function():cs.NativeArray<cs.system.io.FileSystemInfo> {})
	@:overload(function(searchPattern:String):cs.NativeArray<cs.system.io.FileSystemInfo> {})
	@:overload(function(searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<cs.system.io.FileSystemInfo> {})
	/**
	 * Returns an array of strongly typed  entries representing all the files and
	 * subdirectories in a directory.
	 * @return An array of strongly typed  entries.
	 */
	function GetFileSystemInfos(searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<cs.system.io.FileSystemInfo>;
	/**
	 * Moves a  instance and its contents to a new path.
	 * @param destDirName The name and path to which to move this directory. The
	 * destination cannot be another disk volume or a directory with the identical
	 * name. It can be an existing directory to which you want to add this directory as
	 * a subdirectory.
	 */
	function MoveTo(destDirName:String):Void;
	/**
	 * Returns the original path that was passed by the user.
	 * @return The original path that was passed by the user.
	 */
	function ToString():String;
}
