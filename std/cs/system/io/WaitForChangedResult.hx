package cs.system.io;

/** Contains information on the change that occurred. */
@:native("System.IO.WaitForChangedResult")
extern class WaitForChangedResult extends cs.system.ValueType {
	/**
	 * Gets or sets the type of change that occurred.
	 * @return One of the  values.
	 */
	var ChangeType(default, default):cs.system.io.WatcherChangeTypes;
	/**
	 * Gets or sets the name of the file or directory that changed.
	 * @return The name of the file or directory that changed.
	 */
	var Name(default, default):String;
	/**
	 * Gets or sets the original name of the file or directory that was renamed.
	 * @return The original name of the file or directory that was renamed.
	 */
	var OldName(default, default):String;
	/**
	 * Gets or sets a value indicating whether the wait operation timed out.
	 * @return if the  method timed out; otherwise, .
	 */
	var TimedOut(default, default):Bool;
}
