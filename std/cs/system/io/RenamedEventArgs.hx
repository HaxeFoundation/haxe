package cs.system.io;

/** Provides data for the  event. */
@:native("System.IO.RenamedEventArgs")
extern class RenamedEventArgs extends cs.system.io.FileSystemEventArgs {
	/**
	 * Gets the previous fully qualified path of the affected file or directory.
	 * @return The previous fully qualified path of the affected file or directory.
	 */
	var OldFullPath(default, never):String;
	/**
	 * Gets the old name of the affected file or directory.
	 * @return The previous name of the affected file or directory.
	 */
	var OldName(default, never):String;
	function new(changeType:cs.system.io.WatcherChangeTypes, directory:String, name:String, oldName:String):Void;
}
