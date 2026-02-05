package cs.system.io;

/** Listens to the file system change notifications and raises events when a directory, or file in a directory, changes. */
@:native("System.IO.FileSystemWatcher")
extern class FileSystemWatcher extends cs.system.componentmodel.Component {
	/**
	 * Gets or sets a value indicating whether the component is enabled.
	 * @return if the component is enabled; otherwise, . The default is . If you are
	 * using the component on a designer in Visual Studio 2005, the default is .
	 */
	var EnableRaisingEvents(default, default):Bool;
	/**
	 * Gets or sets the filter string used to determine what files are monitored in a
	 * directory.
	 * @return The filter string. The default is "*.*" (Watches all files.)
	 */
	var Filter(default, default):String;
	/**
	 * Gets or sets a value indicating whether subdirectories within the specified path
	 * should be monitored.
	 * @return if you want to monitor subdirectories; otherwise, . The default is .
	 */
	var IncludeSubdirectories(default, default):Bool;
	/**
	 * Gets or sets the size (in bytes) of the internal buffer.
	 * @return The internal buffer size in bytes. The default is 8192 (8 KB).
	 */
	var InternalBufferSize(default, default):Int;
	/**
	 * Gets or sets the type of changes to watch for.
	 * @return One of the  values. The default is the bitwise OR combination of , , and
	 * .
	 */
	var NotifyFilter(default, default):cs.system.io.NotifyFilters;
	/**
	 * Gets or sets the path of the directory to watch.
	 * @return The path to monitor. The default is an empty string ("").
	 */
	var Path(default, default):String;
	/**
	 * Gets or sets the object used to marshal the event handler calls issued as a
	 * result of a directory change.
	 * @return The  that represents the object used to marshal the event handler calls
	 * issued as a result of a directory change. The default is .
	 */
	var SynchronizingObject(default, default):cs.system.componentmodel.ISynchronizeInvoke;
	@:overload(function():Void {})
	@:overload(function(path:String):Void {})
	function new(path:String, filter:String):Void;
	/** Begins the initialization of a  used on a form or used by another component. The initialization occurs at run time. */
	function BeginInit():Void;
	/** Ends the initialization of a  used on a form or used by another component. The initialization occurs at run time. */
	function EndInit():Void;
	@:overload(function(changeType:cs.system.io.WatcherChangeTypes):cs.system.io.WaitForChangedResult {})
	/**
	 * A synchronous method that returns a structure that contains specific information
	 * on the change that occurred, given the type of change you want to monitor.
	 * @param changeType The  to watch for.
	 * @return A  that contains specific information on the change that occurred.
	 */
	function WaitForChanged(changeType:cs.system.io.WatcherChangeTypes, timeout:Int):cs.system.io.WaitForChangedResult;
}
