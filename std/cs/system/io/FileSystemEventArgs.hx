package cs.system.io;

/** Provides data for the directory events: , , . */
@:native("System.IO.FileSystemEventArgs")
extern class FileSystemEventArgs extends cs.system.EventArgs {
	/**
	 * Gets the type of directory event that occurred.
	 * @return One of the  values that represents the kind of change detected in the
	 * file system.
	 */
	var ChangeType(default, never):cs.system.io.WatcherChangeTypes;
	/**
	 * Gets the fully qualified path of the affected file or directory.
	 * @return The path of the affected file or directory.
	 */
	var FullPath(default, never):String;
	/**
	 * Gets the name of the affected file or directory.
	 * @return The name of the affected file or directory.
	 */
	var Name(default, never):String;
	function new(changeType:cs.system.io.WatcherChangeTypes, directory:String, name:String):Void;
}
