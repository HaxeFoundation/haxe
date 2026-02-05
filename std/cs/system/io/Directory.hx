package cs.system.io;

/** Exposes static methods for creating, moving, and enumerating through directories and subdirectories. This class cannot be inherited. */
@:native("System.IO.Directory")
extern class Directory {
	/**
	 * Creates all directories and subdirectories in the specified path unless they
	 * already exist.
	 * @param path The directory to create.
	 * @return An object that represents the directory at the specified path. This
	 * object is returned regardless of whether a directory at the specified path
	 * already exists.
	 */
	static function CreateDirectory(path:String):cs.system.io.DirectoryInfo;
	@:overload(function(path:String):Void {})
	/**
	 * Deletes an empty directory from a specified path.
	 * @param path The name of the empty directory to remove. This directory must be
	 * writable and empty.
	 */
	static function Delete(path:String, recursive:Bool):Void;
	@:overload(function(path:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<String> {})
	/**
	 * Returns an enumerable collection of directory names in a specified path.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An enumerable collection of the full names (including paths) for the
	 * directories in the directory specified by .
	 */
	static function EnumerateDirectories(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<String>;
	@:overload(function(path:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<String> {})
	/**
	 * Returns an enumerable collection of file names in a specified path.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An enumerable collection of the full names (including paths) for the
	 * files in the directory specified by .
	 */
	static function EnumerateFiles(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<String>;
	@:overload(function(path:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String):cs.system.collections.generic.IEnumerable<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.system.collections.generic.IEnumerable<String> {})
	/**
	 * Returns an enumerable collection of file names and directory names in a
	 * specified path.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An enumerable collection of file-system entries in the directory
	 * specified by .
	 */
	static function EnumerateFileSystemEntries(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.system.collections.generic.IEnumerable<String>;
	/**
	 * Determines whether the given path refers to an existing directory on disk.
	 * @param path The path to test.
	 * @return if  refers to an existing directory;  if the directory does not exist or
	 * an error occurs when trying to determine if the specified directory exists.
	 */
	static function Exists(path:String):Bool;
	/**
	 * Gets the creation date and time of a directory.
	 * @param path The path of the directory.
	 * @return A structure that is set to the creation date and time for the specified
	 * directory. This value is expressed in local time.
	 */
	static function GetCreationTime(path:String):cs.system.DateTime;
	/**
	 * Gets the creation date and time, in Coordinated Universal Time (UTC) format, of
	 * a directory.
	 * @param path The path of the directory.
	 * @return A structure that is set to the creation date and time for the specified
	 * directory. This value is expressed in UTC time.
	 */
	static function GetCreationTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Gets the current working directory of the application.
	 * @return A string that contains the absolute path of the current working
	 * directory, and does not end with a backslash (\).
	 */
	static function GetCurrentDirectory():String;
	@:overload(function(path:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<String> {})
	/**
	 * Returns the names of subdirectories (including their paths) in the specified
	 * directory.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An array of the full names (including paths) of subdirectories in the
	 * specified path, or an empty array if no directories are found.
	 */
	static function GetDirectories(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<String>;
	/**
	 * Returns the volume information, root information, or both for the specified
	 * path.
	 * @param path The path of a file or directory.
	 * @return A string that contains the volume information, root information, or both
	 * for the specified path.
	 */
	static function GetDirectoryRoot(path:String):String;
	@:overload(function(path:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<String> {})
	/**
	 * Returns the names of files (including their paths) in the specified directory.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An array of the full names (including paths) for the files in the
	 * specified directory, or an empty array if no files are found.
	 */
	static function GetFiles(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<String>;
	@:overload(function(path:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String):cs.NativeArray<String> {})
	@:overload(function(path:String, searchPattern:String, enumerationOptions:cs.system.io.EnumerationOptions):cs.NativeArray<String> {})
	/**
	 * Returns the names of all files and subdirectories in a specified path.
	 * @param path The relative or absolute path to the directory to search. This
	 * string is not case-sensitive.
	 * @return An array of the names of files and subdirectories in the specified
	 * directory, or an empty array if no files or subdirectories are found.
	 */
	static function GetFileSystemEntries(path:String, searchPattern:String, searchOption:cs.system.io.SearchOption):cs.NativeArray<String>;
	/**
	 * Returns the date and time the specified file or directory was last accessed.
	 * @param path The file or directory for which to obtain access date and time
	 * information.
	 * @return A structure that is set to the date and time the specified file or
	 * directory was last accessed. This value is expressed in local time.
	 */
	static function GetLastAccessTime(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time, in Coordinated Universal Time (UTC) format, that the
	 * specified file or directory was last accessed.
	 * @param path The file or directory for which to obtain access date and time
	 * information.
	 * @return A structure that is set to the date and time the specified file or
	 * directory was last accessed. This value is expressed in UTC time.
	 */
	static function GetLastAccessTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time the specified file or directory was last written to.
	 * @param path The file or directory for which to obtain modification date and time
	 * information.
	 * @return A structure that is set to the date and time the specified file or
	 * directory was last written to. This value is expressed in local time.
	 */
	static function GetLastWriteTime(path:String):cs.system.DateTime;
	/**
	 * Returns the date and time, in Coordinated Universal Time (UTC) format, that the
	 * specified file or directory was last written to.
	 * @param path The file or directory for which to obtain modification date and time
	 * information.
	 * @return A structure that is set to the date and time the specified file or
	 * directory was last written to. This value is expressed in UTC time.
	 */
	static function GetLastWriteTimeUtc(path:String):cs.system.DateTime;
	/**
	 * Retrieves the names of the logical drives on this computer in the form "<drive
	 * letter>:\".
	 * @return The logical drives on this computer.
	 */
	static function GetLogicalDrives():cs.NativeArray<String>;
	/**
	 * Retrieves the parent directory of the specified path, including both absolute
	 * and relative paths.
	 * @param path The path for which to retrieve the parent directory.
	 * @return The parent directory, or  if  is the root directory, including the root
	 * of a UNC server or share name.
	 */
	static function GetParent(path:String):cs.system.io.DirectoryInfo;
	/**
	 * Moves a file or a directory and its contents to a new location.
	 * @param sourceDirName The path of the file or directory to move.
	 * @param destDirName The path to the new location for . If  is a file, then  must
	 * also be a file name.
	 */
	static function Move(sourceDirName:String, destDirName:String):Void;
	/**
	 * Sets the creation date and time for the specified file or directory.
	 * @param path The file or directory for which to set the creation date and time
	 * information.
	 * @param creationTime The date and time the file or directory was last written to.
	 * This value is expressed in local time.
	 */
	static function SetCreationTime(path:String, creationTime:cs.system.DateTime):Void;
	/**
	 * Sets the creation date and time, in Coordinated Universal Time (UTC) format, for
	 * the specified file or directory.
	 * @param path The file or directory for which to set the creation date and time
	 * information.
	 * @param creationTimeUtc The date and time the directory or file was created. This
	 * value is expressed in local time.
	 */
	static function SetCreationTimeUtc(path:String, creationTimeUtc:cs.system.DateTime):Void;
	/**
	 * Sets the application's current working directory to the specified directory.
	 * @param path The path to which the current working directory is set.
	 */
	static function SetCurrentDirectory(path:String):Void;
	/**
	 * Sets the date and time the specified file or directory was last accessed.
	 * @param path The file or directory for which to set the access date and time
	 * information.
	 * @param lastAccessTime An object that contains the value to set for the access
	 * date and time of . This value is expressed in local time.
	 */
	static function SetLastAccessTime(path:String, lastAccessTime:cs.system.DateTime):Void;
	/**
	 * Sets the date and time, in Coordinated Universal Time (UTC) format, that the
	 * specified file or directory was last accessed.
	 * @param path The file or directory for which to set the access date and time
	 * information.
	 * @param lastAccessTimeUtc An object that  contains the value to set for the
	 * access date and time of . This value is expressed in UTC time.
	 */
	static function SetLastAccessTimeUtc(path:String, lastAccessTimeUtc:cs.system.DateTime):Void;
	/**
	 * Sets the date and time a directory was last written to.
	 * @param path The path of the directory.
	 * @param lastWriteTime The date and time the directory was last written to. This
	 * value is expressed in local time.
	 */
	static function SetLastWriteTime(path:String, lastWriteTime:cs.system.DateTime):Void;
	/**
	 * Sets the date and time, in Coordinated Universal Time (UTC) format, that a
	 * directory was last written to.
	 * @param path The path of the directory.
	 * @param lastWriteTimeUtc The date and time the directory was last written to.
	 * This value is expressed in UTC time.
	 */
	static function SetLastWriteTimeUtc(path:String, lastWriteTimeUtc:cs.system.DateTime):Void;
}
