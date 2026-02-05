package cs.system.io;

/** Provides the base class for both  and  objects. */
@:native("System.IO.FileSystemInfo")
extern class FileSystemInfo extends cs.system.MarshalByRefObject {
	/**
	 * Gets or sets the attributes for the current file or directory.
	 * @return of the current .
	 */
	var Attributes(default, default):cs.system.io.FileAttributes;
	/**
	 * Gets or sets the creation time of the current file or directory.
	 * @return The creation date and time of the current  object.
	 */
	var CreationTime(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the creation time, in coordinated universal time (UTC), of the
	 * current file or directory.
	 * @return The creation date and time in UTC format of the current  object.
	 */
	var CreationTimeUtc(default, default):cs.system.DateTime;
	/**
	 * Gets a value indicating whether the file or directory exists.
	 * @return if the file or directory exists; otherwise, .
	 */
	var Exists(default, never):Bool;
	/**
	 * Gets the string representing the extension part of the file.
	 * @return A string containing the  extension.
	 */
	var Extension(default, never):String;
	/**
	 * Gets the full path of the directory or file.
	 * @return A string containing the full path.
	 */
	var FullName(default, never):String;
	/**
	 * Gets or sets the time the current file or directory was last accessed.
	 * @return The time that the current file or directory was last accessed.
	 */
	var LastAccessTime(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the time, in coordinated universal time (UTC), that the current
	 * file or directory was last accessed.
	 * @return The UTC time that the current file or directory was last accessed.
	 */
	var LastAccessTimeUtc(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the time when the current file or directory was last written to.
	 * @return The time the current file was last written.
	 */
	var LastWriteTime(default, default):cs.system.DateTime;
	/**
	 * Gets or sets the time, in coordinated universal time (UTC), when the current
	 * file or directory was last written to.
	 * @return The UTC time when the current file was last written to.
	 */
	var LastWriteTimeUtc(default, default):cs.system.DateTime;
	/**
	 * For files, gets the name of the file. For directories, gets the name of the last
	 * directory in the hierarchy if a hierarchy exists. Otherwise, the  property gets
	 * the name of the directory.
	 * @return A string that is the name of the parent directory, the name of the last
	 * directory in the hierarchy, or the name of a file, including the file name
	 * extension.
	 */
	var Name(default, never):String;
	/** Deletes a file or directory. */
	function Delete():Void;
	/**
	 * Sets the  object with the file name and additional exception information.
	 * @param info The  that holds the serialized object data about the exception being
	 * thrown.
	 * @param context The  that contains contextual information about the source or
	 * destination.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/** Refreshes the state of the object. */
	function Refresh():Void;
	function ToString():String;
}
